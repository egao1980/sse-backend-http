(in-package #:sse-backend-http)

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

(defmethod sse-protocol:backend-open-sse ((backend http-sse-backend) url
                                          &key last-event-id headers timeout
                                            (method :get) content)
  (declare (ignore backend))
  (%ensure-http-backend)
  (let* ((req-headers (%sse-request-headers last-event-id headers))
         (args (append (list method url
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

(defmethod sse-protocol:backend-serve-sse ((backend http-sse-backend) handler
                                           &key host port path)
  (declare (ignore handler host port path))
  (error 'sse-protocol:sse-error
         :message "sse-backend-http is consume-only — use sse-backend-clack for serve-sse"))

(use-http-sse-backend)
