(in-package #:sse-backend-http)

;;; Reconnect/retry lives here. sse-protocol only frames and parses
;;; `retry:` onto SSE-READER-RETRY.

(defparameter *sse-reconnect-default-ms* 3000
  "WHATWG default reconnect delay when the stream has no retry field.")

(defclass http-sse-backend (sse-protocol:sse-backend) ())

(defun make-http-sse-backend ()
  (make-instance 'http-sse-backend))

(defun use-http-sse-backend ()
  (setf sse-protocol:*sse-backend* (make-http-sse-backend)))

(defun %header-alist (headers)
  (cond
    ((null headers) '())
    ((hash-table-p headers)
     (let ((out '()))
       (maphash (lambda (k v)
                  (when v
                    (push (cons (string-downcase (string k))
                                (if (stringp v) v (princ-to-string v)))
                          out)))
                headers)
       (nreverse out)))
    (t
     (loop for pair in headers
           for name = (string-downcase (string (if (consp pair) (car pair) pair)))
           for value = (if (consp pair) (cdr pair) nil)
           when value
             collect (cons name (if (stringp value) value (princ-to-string value)))))))

(defun %sse-request-headers (last-event-id headers)
  (append '(("accept" . "text/event-stream"))
          (when last-event-id
            (list (cons "last-event-id" last-event-id)))
          (%header-alist headers)))

(defun %ensure-http-backend ()
  (or http-protocol:*http-backend*
      (error 'sse-protocol:sse-error
             :message "*http-backend* is nil — bind http-backend-dexador or http-backend-async")))

(defun %open-sse-once (url &key last-event-id headers timeout (method :get) content)
  (%ensure-http-backend)
  (let* ((req-headers (%sse-request-headers last-event-id headers))
         (args (append (list (or method :get) url
                             :want-stream t
                             :headers req-headers
                             :accept-encoding nil
                             :decompress nil)
                       (when timeout (list :timeout timeout))
                       (when content (list :content content))))
         (res (apply #'http:request args))
         (status (http-protocol:response-status res)))
    (unless (<= 200 status 299)
      (error 'sse-protocol:sse-error
             :message (format nil "SSE HTTP ~a for ~a" status url)))
    (make-instance 'sse-protocol:sse-connection
                   :url url
                   :reader (sse-protocol:make-sse-reader
                            (http-protocol:body-stream res)
                            :last-event-id last-event-id)
                   :close (lambda (&key abort)
                            (let ((body (http-protocol:response-body res)))
                              (when (streamp body)
                                (ignore-errors (close body :abort abort))))
                            (ignore-errors
                              (http-protocol:release-response-connection
                               res :abort abort))))))

(defclass reconnecting-sse-connection (sse-protocol:sse-connection)
  ((backend :initarg :backend :reader reconnecting-backend)
   (headers :initarg :headers :initform nil :reader reconnecting-headers)
   (timeout :initarg :timeout :initform nil :reader reconnecting-timeout)
   (method :initarg :method :initform :get :reader reconnecting-method)
   (content :initarg :content :initform nil :reader reconnecting-content)
   (default-retry :initarg :default-retry
                  :initform *sse-reconnect-default-ms*
                  :reader reconnecting-default-retry)
   (reconnect-limit :initarg :reconnect-limit :initform nil
                    :reader reconnecting-limit)
   (reconnect-count :initform 0 :accessor reconnecting-count)
   (closed :initform nil :accessor reconnecting-closed-p)
   (inner-close :initarg :inner-close :initform nil
                :accessor reconnecting-inner-close)))

(defun %retry-seconds (connection)
  (/ (float (or (sse-protocol:sse-reader-retry
                 (sse-protocol:sse-connection-reader connection))
                (reconnecting-default-retry connection)
                *sse-reconnect-default-ms*))
     1000.0))

(defun %swap-connection (target fresh)
  (let ((retry (sse-protocol:sse-reader-retry
                (sse-protocol:sse-connection-reader target))))
    (setf (slot-value target 'sse-protocol::reader)
          (sse-protocol:sse-connection-reader fresh)
          (reconnecting-inner-close target)
          (sse-protocol:sse-connection-closer fresh))
    (when retry
      (setf (sse-protocol:sse-reader-retry
             (sse-protocol:sse-connection-reader target))
            retry))
    target))

(defun %close-inner (connection &key abort)
  (let ((fn (reconnecting-inner-close connection)))
    (when fn
      (ignore-errors (funcall fn :abort abort))
      (setf (reconnecting-inner-close connection) nil)))
  (let ((reader (sse-protocol:sse-connection-reader connection)))
    (when reader
      (ignore-errors (sse-protocol:close-sse-reader reader :abort abort)))))

(defun %reconnect (connection)
  (when (reconnecting-closed-p connection)
    (return-from %reconnect nil))
  (let ((limit (reconnecting-limit connection)))
    (when (and limit (>= (reconnecting-count connection) limit))
      (return-from %reconnect nil)))
  (incf (reconnecting-count connection))
  (let ((delay (%retry-seconds connection)))
    (%close-inner connection :abort t)
    (when (plusp delay)
      (sleep delay)))
  (when (reconnecting-closed-p connection)
    (return-from %reconnect nil))
  (let ((fresh (%open-sse-once (sse-protocol:sse-connection-url connection)
                               :last-event-id
                               (sse-protocol:sse-connection-last-event-id connection)
                               :headers (reconnecting-headers connection)
                               :timeout (reconnecting-timeout connection)
                               :method (reconnecting-method connection)
                               :content (reconnecting-content connection))))
    (%swap-connection connection fresh)
    t))

(defmethod sse-protocol:sse-connection-read-event
    ((connection reconnecting-sse-connection) &key include-empty include-keepalives)
  (loop
    (when (reconnecting-closed-p connection)
      (return nil))
    (handler-case
        (let ((ev (call-next-method connection
                                    :include-empty include-empty
                                    :include-keepalives include-keepalives)))
          (if ev
              (return ev)
              (unless (%reconnect connection)
                (return nil))))
      (end-of-file ()
        (unless (%reconnect connection)
          (return nil)))
      (stream-error ()
        (unless (%reconnect connection)
          (return nil))))))

(defun %wrap-close (connection)
  (setf (reconnecting-inner-close connection)
        (sse-protocol:sse-connection-closer connection))
  (setf (slot-value connection 'sse-protocol::close)
        (lambda (&key abort)
          (setf (reconnecting-closed-p connection) t)
          (%close-inner connection :abort abort)))
  connection)

(defun %make-reconnecting (backend conn &key headers timeout method content
                                          default-retry reconnect-limit)
  (%wrap-close
   (change-class conn 'reconnecting-sse-connection
                 :backend backend
                 :headers headers
                 :timeout timeout
                 :method method
                 :content content
                 :default-retry (or default-retry *sse-reconnect-default-ms*)
                 :reconnect-limit reconnect-limit)))

(defmethod sse-protocol:backend-open-sse ((backend http-sse-backend) url
                                          &key last-event-id headers timeout
                                            (method :get) content
                                            reconnect default-retry
                                            reconnect-limit)
  (declare (ignore backend))
  (let ((conn (%open-sse-once url
                              :last-event-id last-event-id
                              :headers headers
                              :timeout timeout
                              :method method
                              :content content)))
    (if reconnect
        (%make-reconnecting (make-http-sse-backend) conn
                            :headers headers
                            :timeout timeout
                            :method method
                            :content content
                            :default-retry default-retry
                            :reconnect-limit reconnect-limit)
        conn)))

(defun open-sse-with-reconnect (url &key last-event-id headers timeout
                                      (method :get) content
                                      (default-retry *sse-reconnect-default-ms*)
                                      reconnect-limit)
  "OPEN-SSE with :RECONNECT T. On read error/EOF, sleep
   (or SSE-READER-RETRY DEFAULT-RETRY) ms and reopen with Last-Event-ID."
  (sse-protocol:open-sse url
                         :last-event-id last-event-id
                         :headers headers
                         :timeout timeout
                         :method method
                         :content content
                         :reconnect t
                         :default-retry default-retry
                         :reconnect-limit reconnect-limit))

(defmethod sse-protocol:backend-serve-sse ((backend http-sse-backend) handler
                                           &key host port path)
  (declare (ignore handler host port path))
  (error 'sse-protocol:sse-error
         :message "sse-backend-http is consume-only — use sse-backend-clack for serve-sse"))

(use-http-sse-backend)
