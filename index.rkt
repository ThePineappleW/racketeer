#lang Racket

; Dependencies
(require dotenv
         racket-cord
         racket/contract
         racket/base)

; Commands
; Place all your command files in the commands/ folder.
; Add your new commands to this require block.
(require "commands/hello.rkt"
         "commands/wd.rkt"
         "commands/lol.rkt")

(dotenv-load!)
(define BOT-TOKEN (getenv "BOT_TOKEN"))
(define INTENTS (list intent-guilds
                      intent-guild-messages
                      intent-guild-message-reactions))

(define myclient (make-client BOT-TOKEN #:auto-shard #t #:intents INTENTS))

(struct role [emoji name color])

(define TRUSTED-USER-IDS (list 
                          "269990823796539392" ; Emery
                          ))

(define (trusted-author? payload)
  (member (hash-ref (hash-ref payload 'author) 'id) TRUSTED-USER-IDS))



(define POLL-MARKER "-=-")


;; Emoji-name -> Role  
(define current-poll-roles (make-hash))



(on-event
 'raw-message-create myclient
 (lambda (ws-client client payload)
   (unless (string=? (hash-ref (hash-ref payload 'author) 'id)
                     (hash-ref (client-user client) 'id))
     (local [(define (cmd? str) (string-prefix? (hash-ref payload 'content) str))]
       (cond
         ; Add commands into this cond.
         ; Follow the form:
         ; [(cmd? "<name with prefix>") (<name> client payload)]
         
         ; !hello : responds with "hi"
         [(cmd? "!hello")
          (cmd/hello client payload)]

         [(cmd? "!id")
          (http:create-message client
                               (hash-ref payload 'channel_id)
                               (hash-ref (hash-ref payload 'author) 'id))]

         [(cmd? "!wd")
          (cmd/wd client payload)]

         [(cmd? "!react")
          (http:create-reaction
           client
           (hash-ref payload 'channel_id)
           (hash-ref payload 'id)
           ":thumbsup:")]
         
         [(cmd? "!debug")
          (println current-poll-roles)]
         
         [(cmd? "!echo")
          (println (hash-ref payload 'content))
          (http:create-message client (hash-ref payload 'channel_id) (hash-ref payload 'content))]

         [(cmd? "!init-poll")
          (if (trusted-author? payload)
              (for-each (λ(r)
                          (hash-set! current-poll-roles (normalize-emoji (role-emoji r))
                                     (http:create-guild-role client
                                                             (hash-ref payload 'guild_id)
                                                             (hash 'name (role-name r)
                                                                   'color (string->number (role-color r))
                                                                   'hoist #t)))
                          (http:create-message client
                                               (hash-ref payload 'channel_id)
                                               (role-emoji r)))
                        (extract-role-assignments (hash-ref payload 'content)))
              (http:create-message client (hash-ref payload 'channel_id) "You are not allowed to use that command!"))]



         )))))

(on-event
 'raw-message-reaction-add myclient
 (lambda (ws-client client payload)
   (let* ([emoji (hash-ref payload 'emoji)]
          [r (hash-ref current-poll-roles (hash-ref emoji 'name))])
     (println emoji)
     (if (should-update-role client payload)
         (http:add-guild-member-role client
                                     (hash-ref payload 'guild_id)
                                     (hash-ref payload 'user_id)
                                     (hash-ref r 'id))
         #f))))

(on-event
 'raw-message-reaction-remove myclient
 (lambda (ws-client client payload)
   (let* ([emoji (hash-ref payload 'emoji)]
          [r (hash-ref current-poll-roles (hash-ref emoji 'name))])
     (println emoji)
     (if (should-update-role client payload)
         (http:remove-guild-member-role client
                                     (hash-ref payload 'guild_id)
                                     (hash-ref payload 'user_id)
                                     (hash-ref r 'id))
         #f))))


;; Reaction-Payload -> Boolean
;; Is does this method contain the poll initializer and was it sent by a trusted user?
;; I.e. Should any reactions on this message assign roles?
(define (should-update-role client react-payload)
  (let ([message (http:get-channel-message client
                                           (hash-ref react-payload 'channel_id)
                                           (hash-ref react-payload 'message_id))])
    (and (member (hash-ref react-payload 'user_id) TRUSTED-USER-IDS)
         (string-contains? (hash-ref message 'content) POLL-MARKER))))

;; String -> List of Role
;; extracts information from messages in the form:
;;|:emoji:|name|color|
(define (extract-role-assignments input)
  (let* ([all-components (filter (λ(s) (> (string-length (string-trim s)) 1))
                                 (rest (string-split input "|")))] ;; discard the message prefix.
         [colors (map (compose
                       (λ(s) (string-append "#x" s))
                       (λ(s) (substring s 1))
                       string-trim)
                      (filter (λ(s) (string-prefix? (string-trim s) "#")) all-components))]
         
         [names (map (λ(x) (string-trim (string-trim x) "\""))
                     (filter (λ(s) (string-prefix? (string-trim s) "\""))
                             all-components))]
         
         [emoji (map (λ(x) (string-trim (string-trim (string-trim x) "{") "}")) (filter (λ(s) (string-prefix? (string-trim s) "{"))
                                                                                        all-components))])
    (println "")
    (println emoji)
    (println names)
    (println colors)
    (map role emoji names colors)))

(define (normalize-emoji emoji)
  (if (string-prefix? emoji "<")
      (string-trim (first (regexp-match #rx":.+:" emoji)) ":")
      emoji))



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