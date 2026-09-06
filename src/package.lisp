(defpackage #:sse-backend-http
  (:use #:cl)
  (:export #:http-sse-backend
           #:make-http-sse-backend
           #:use-http-sse-backend
           #:*sse-reconnect-default-ms*
           #:open-sse-with-reconnect
           #:reconnecting-sse-connection))

(in-package #:sse-backend-http)
