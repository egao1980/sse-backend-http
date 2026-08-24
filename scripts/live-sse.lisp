;;;; Live dogfood: Hunchentoot SSE server × http-protocol client.
;;;; Prefers cl-stack-http + http-backend-async × event-backend-libuv,
;;;; falls back to dexador (sync :want-stream).
;;;;
;;;;   sbcl --load scripts/live-sse.lisp

(setf *debugger-hook*
      (lambda (c h)
        (declare (ignore h))
        (format *error-output* "~&live-sse failed: ~a~%" c)
        (uiop:quit 1)))

(defun %here ()
  (uiop:pathname-directory-pathname
   (or *load-truename* *compile-file-truename* (uiop:getcwd))))

(defun %root ()
  (uiop:pathname-parent-directory-pathname (%here)))

(defun %workspace ()
  (uiop:pathname-parent-directory-pathname (%root)))

(dolist (name '("sse-protocol" "sse-backend-http" "sse-backend-clack"
                "http-protocol" "http-server-protocol" "cl-stack-http"
                "http-backend-async" "http-backend-dexador"
                "event-backend-libuv" "event-protocol"))
  (let ((dir (merge-pathnames (format nil "~a/" name) (%workspace))))
    (when (probe-file dir)
      (pushnew dir asdf:*central-registry* :test #'equal))))

(asdf:load-system "sse-backend-http")
(asdf:load-system "sse-backend-clack")
(asdf:load-system "http-server-backend-hunchentoot")
(asdf:load-system "usocket")

(defun fail (fmt &rest args)
  (apply #'format *error-output* (concatenate 'string "~&FAIL: " fmt "~%") args)
  (uiop:quit 1))

(defun check (pred fmt &rest args)
  (unless pred (apply #'fail fmt args)))

(defun ev (&rest args)
  (apply #'sse-protocol:make-sse-event args))

(defun %free-port ()
  (let* ((sock (usocket:socket-listen "127.0.0.1" 0 :reuseaddress t))
         (port (usocket:get-local-port sock)))
    (usocket:socket-close sock)
    port))

(defun %bind-http-client ()
  (or (ignore-errors
        (asdf:load-system "event-backend-libuv")
        (asdf:load-system "http-backend-async")
        (setf (symbol-value (find-symbol "*EVENT-BACKEND-MAKER*" :http-backend-async))
              (lambda ()
                (funcall (find-symbol "MAKE-LIBUV-BACKEND" :event-backend-libuv))))
        (setf http-protocol:*http-backend*
              (funcall (find-symbol "MAKE-ASYNC-BACKEND" :http-backend-async)))
        (format t "~&; client backend: async × libuv~%")
        t)
      (progn
        (asdf:load-system "http-backend-dexador")
        (setf http-protocol:*http-backend*
              (funcall (find-symbol "MAKE-DEXADOR-BACKEND" :http-backend-dexador)))
        (format t "~&; client backend: dexador~%")
        t)))

(http-server-backend-hunchentoot:use-hunchentoot-backend)
(%bind-http-client)
(sse-backend-http:use-http-sse-backend)

(let* ((port (%free-port))
       (app (sse-backend-clack:make-sse-app
             (lambda (env)
               (if (equal "1" (sse-backend-clack:request-last-event-id env))
                   (list (ev :id "2" :event "tick" :data "resume"))
                   (list (ev :id "1" :data "hello")
                         (ev :event "ping" :data "ok"))))
             :path "/sse"))
       (url (format nil "http://127.0.0.1:~a/sse" port)))
  (http-server-protocol:with-server (s app :host "127.0.0.1" :port port)
    (check (http-server-protocol:running-p s) "server not running")
    (sleep 0.2)
    (let* ((conn (sse-protocol:open-sse url))
           (evs (unwind-protect (sse-protocol:collect-sse-events conn)
                  (sse-protocol:close-sse conn))))
      (check (= 2 (length evs)) "first collect count ~a" (length evs))
      (check (equal "hello" (sse-protocol:sse-event-data (first evs)))
             "first data")
      (check (equal "ok" (sse-protocol:sse-event-data (second evs)))
             "second data"))
    (let* ((conn (sse-protocol:open-sse url :last-event-id "1"))
           (evs (unwind-protect (sse-protocol:collect-sse-events conn)
                  (sse-protocol:close-sse conn))))
      (check (= 1 (length evs)) "resume count ~a" (length evs))
      (check (equal "resume" (sse-protocol:sse-event-data (first evs)))
             "resume data")
      (check (equal "tick" (sse-protocol:sse-event-type (first evs)))
             "resume event"))))

(format t "~&; live-sse ok~%")
(uiop:quit 0)
