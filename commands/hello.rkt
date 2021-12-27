#lang Racket

(require racket-cord)
(provide cmd/hello)

; cmd/hello : Client Payload -> JSexpr	
(define (cmd/hello client payload)
  (http:create-message client (hash-ref payload 'channel_id) "hi"))

