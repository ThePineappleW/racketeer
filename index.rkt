#lang Racket

; Dependencies
(require dotenv
         racket-cord
         racket/contract)

; Commands
(require "commands/hello.rkt")

(dotenv-load!)
(define BOT-TOKEN (getenv "BOT_TOKEN"))
(define myclient (make-client BOT-TOKEN #:auto-shard #t #:intents (list intent-guilds intent-guild-messages)))

(on-event
 'raw-message-create myclient
 (lambda (ws-client client payload)
   (unless (string=? (hash-ref (hash-ref payload 'author) 'id) (user-id client))
     (cond
       ; !hello : responds with hi
       [(string-prefix? (hash-ref payload 'content) "!hello")
        (cmd/hello client payload)]


       
       ))))
 
(define (user-id client)
  (hash-ref (client-user client) 'id))

(define/contract (make-attachment data name type)
  (-> bytes? (or/c string? bytes?) (or/c symbol? string? bytes?) http:attachment?)
  (http:attachment data (~a type) name))

(define dr (make-log-receiver discord-logger 'debug))
 
(thread
 (thunk
  (let loop ()
    (let ([v (sync dr)])
      (printf "[~a] ~a\n" (vector-ref v 0)
              (vector-ref v 1)))
    (loop))))
 
(start-client myclient)