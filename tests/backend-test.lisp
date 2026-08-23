(in-package #:sse-backend-http/tests)

(deftest backend-class
  (ok (typep (sse-backend-http:make-http-sse-backend) 'sse-backend-http:http-sse-backend)))
