# sse-backend-http

http-protocol client backend for sse-protocol.

Part of [cl-stack](https://github.com/egao1980/cl-stack) agent-wire ([brief](https://github.com/egao1980/cl-stack/blob/main/docs/capabilities/agent-wire.md)).

```lisp
(asdf:load-system "sse-backend-http")

(setf http-protocol:*http-backend* (http-backend-dexador:make-dexador-backend))
(sse-backend-http:use-http-sse-backend)

(let ((conn (sse-protocol:open-sse "http://127.0.0.1:8080/sse")))
  (unwind-protect (sse-protocol:collect-sse-events conn)
    (sse-protocol:close-sse conn)))
```

Live dogfood (Hunchentoot + cl-stack-http / dexador):

```
sbcl --load scripts/live-sse.lisp
```

CI: `setup-client` + `setup-roswell` + `scripts/ci-install.lisp` / `ci-test.lisp` (OCI only, no Quicklisp).

## License

MIT
