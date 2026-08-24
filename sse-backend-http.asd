(defsystem "sse-backend-http"
  :version "0.1.0"
  :description "http-protocol client backend for sse-protocol"
  :author "egao1980"
  :license "MIT"
  :depends-on ("sse-protocol" "http-protocol")
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "backend"))
  :in-order-to ((test-op (test-op "sse-backend-http/tests"))))

(defsystem "sse-backend-http/tests"
  :depends-on ("sse-backend-http"
               "http-backend-dexador"
               "http-server-backend-hunchentoot"
               "rove"
               "usocket")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "backend-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
