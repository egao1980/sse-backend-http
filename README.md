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

;; Reconnect on EOF / read error (protocol stays framing-only):
(sse-protocol:open-sse url :reconnect t :default-retry 3000 :reconnect-limit 3)
;; or
(sse-backend-http:open-sse-with-reconnect url :default-retry 50 :reconnect-limit 1)
```

On read error or EOF the client sleeps `(or sse-reader-retry default-ms)` milliseconds and reopens with `Last-Event-ID`. `close-sse` stops further reconnects. `sse-protocol` only parses the `retry:` field.

Live dogfood (Hunchentoot + cl-stack-http / dexador):

```
sbcl --load scripts/live-sse.lisp
```

CI: canned [`cl-repository`](https://github.com/egao1980/cl-repository) (`test-system.yml` / `setup-client` + `ci`). Deps from `ghcr.io/egao1980/cl-systems`.

## License

MIT
