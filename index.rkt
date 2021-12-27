#lang Racket

; Dependencies
(require dotenv
         racket-cord
         racket/contract)

; Commands
; Place all your command files in the commands/ folder.
; For convenience, just write the name of the file within that folder here.
; We then automatically append the commands/ folder to the directory name,
; so you don't have to keep writing it.
; Note that if you have subfolders within commands/, you DO need to include those
; in the directory name.
(define cmd-files (list
                   "hello.rkt"))
(require (map (λ (x) (string-append "commands/")) cmd-files))

(dotenv-load!)
(define BOT-TOKEN (getenv "BOT_TOKEN"))
(define myclient (make-client BOT-TOKEN #:auto-shard #t #:intents (list intent-guilds intent-guild-messages)))

(on-event
 'raw-message-create myclient
 (lambda (ws-client client payload)
   (unless (string=? (hash-ref (hash-ref payload 'author) 'id) (user-id client))
     ; generalized form of the command checker
     (local [(define (cmd? str) (string-prefix? (hash-ref payload 'content) str))]
       (cond
         ; !hello : responds with "hi"
         [(cmd? "!hello")
          (cmd/hello client payload)]


       
         )))))
 
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