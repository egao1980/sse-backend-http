(in-package #:sse-backend-http/tests)

(defun ev (&rest args)
  (apply #'sse-protocol:make-sse-event args))

(defun %free-port ()
  (let* ((sock (usocket:socket-listen "127.0.0.1" 0 :reuseaddress t))
         (port (usocket:get-local-port sock)))
    (usocket:socket-close sock)
    port))

(defun %bind ()
  (http-server-backend-hunchentoot:use-hunchentoot-backend)
  (setf http-protocol:*http-backend*
        (http-backend-dexador:make-dexador-backend))
  (sse-backend-http:use-http-sse-backend))

(defun %sse-app (handler)
  (lambda (env)
    (let* ((headers (getf env :headers))
           (last (and headers (gethash "last-event-id" headers)))
           (events (funcall handler last)))
      (list 200
            '(:content-type "text/event-stream; charset=utf-8"
              :cache-control "no-cache")
            (mapcar #'sse-protocol:encode-sse-event events)))))

(deftest backend-class
  (ok (typep (sse-backend-http:make-http-sse-backend)
             'sse-backend-http:http-sse-backend)))

(deftest serve-sse-rejected
  (let ((sse-protocol:*sse-backend* (sse-backend-http:make-http-sse-backend)))
    (ok (signals (sse-protocol:serve-sse (lambda (env) (declare (ignore env)) nil))
                 'sse-protocol:sse-error))))

(deftest open-sse-requires-http-backend
  (let ((http-protocol:*http-backend* nil)
        (sse-protocol:*sse-backend* (sse-backend-http:make-http-sse-backend)))
    (ok (signals (sse-protocol:open-sse "http://127.0.0.1/sse")
                 'sse-protocol:sse-error))))

(deftest live-open-sse
  (%bind)
  (let* ((port (%free-port))
         (app (%sse-app (lambda (last)
                          (declare (ignore last))
                          (list (ev :id "1" :data "hello")
                                (ev :event "ping" :data "ok"))))))
    (http-server-protocol:with-server (s app :host "127.0.0.1" :port port)
      (sleep 0.2)
      (let* ((conn (sse-protocol:open-sse
                    (format nil "http://127.0.0.1:~a/" port)))
             (evs (unwind-protect
                       (sse-protocol:collect-sse-events conn)
                    (sse-protocol:close-sse conn))))
        (ok (= 2 (length evs)))
        (ok (equal "hello" (sse-protocol:sse-event-data (first evs))))
        (ok (equal "ok" (sse-protocol:sse-event-data (second evs))))
        (ok (equal "1" (sse-protocol:sse-event-id (second evs))))))))

(deftest live-open-sse-last-event-id
  (%bind)
  (let* ((port (%free-port))
         (app (%sse-app (lambda (last)
                          (if (equal last "1")
                              (list (ev :id "2" :data "resume"))
                              (list (ev :id "1" :data "first")))))))
    (http-server-protocol:with-server (s app :host "127.0.0.1" :port port)
      (sleep 0.2)
      (let* ((url (format nil "http://127.0.0.1:~a/" port))
             (a (sse-protocol:open-sse url))
             (evs1 (unwind-protect (sse-protocol:collect-sse-events a)
                     (sse-protocol:close-sse a)))
             (b (sse-protocol:open-sse url :last-event-id "1"))
             (evs2 (unwind-protect (sse-protocol:collect-sse-events b)
                     (sse-protocol:close-sse b))))
        (ok (equal "first" (sse-protocol:sse-event-data (first evs1))))
        (ok (equal "resume" (sse-protocol:sse-event-data (first evs2))))))))

(deftest live-http-error
  (%bind)
  (let ((port (%free-port)))
    (http-server-protocol:with-server
        (s (lambda (env)
             (declare (ignore env))
             '(404 (:content-type "text/plain") ("no")))
           :host "127.0.0.1" :port port)
      (sleep 0.2)
      (ok (signals (sse-protocol:open-sse (format nil "http://127.0.0.1:~a/" port))
                   'sse-protocol:sse-error)))))
