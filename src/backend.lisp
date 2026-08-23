(in-package #:sse-backend-http)

(defclass http-sse-backend (sse-protocol:sse-backend) ())

(defun make-http-sse-backend ()
  (make-instance 'http-sse-backend))

(defun use-http-sse-backend ()
  (setf sse-protocol:*sse-backend* (make-http-sse-backend)))
