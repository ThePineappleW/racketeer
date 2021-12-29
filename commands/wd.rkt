#lang Racket

(require 2htdp/image
         racket-cord
         racket/contract)

(provide cmd/wd)

; cmd/wd : Client Payload -> JSexpr	
(define (cmd/wd client payload)
  (local [(define args (rest (string-split (hash-ref payload 'content))))
          (define img (string->wd (string-join args)))]
    (http:create-message
     client (hash-ref payload 'channel_id)
     #:file (make-attachment (file->bytes "wd.png")
                             "webdings.png"
                             "image/png"))))

; string->wd : String -> Boolean
; Translates a string into a webdings image, and saves it as "wd.png"
(define (string->wd str)
  (local [(define chars (map list->string (explode str)))
          (define img
            (foldl (λ (c sf) (beside sf (match-wd c) (rectangle 5 1 "solid" "transparent"))) empty-image chars))]
    (save-image
     (overlay img
              (empty-scene (+ 10 (image-width img)) (+ 10 (image-height img))))
     "wd.png")))

; match-wd : String -> Image
; Converts a letter into the appropriate webding
(define (match-wd str)
  (cond [(> (string-length str) 1) (error "Error: The string is too long.")]
        [(string=? str " ")
         (rectangle 15 20 "solid" "transparent")]
        [else (bitmap/file (string-append "webdings/" str ".png"))]))

; explode : String -> [List-of [List-of Char]]
; Separates a String into its Chars
(define (explode str)
  (cond [(zero? (string-length str)) '()]
        [(positive? (string-length str))
         (cons (list (string-ref str 0)) (explode (substring str 1)))]))

(define/contract (make-attachment data name type)
  (-> bytes? (or/c string? bytes?) (or/c symbol? string? bytes?) http:attachment?)
  (http:attachment data (~a type) name))
