#lang Racket

(provide (struct-out command))
; This file declares the generalized structure of a Racketeer chat command.

; name : String
; The name of the command, without the bot prefix.

; args : [List-of String]
; The list of expected arguments, in the appropriate order.
; Optional arguments should be written as <arg>.

; desc : String
; A description of the command's functionality.

; proc : [Client JSexpr -> JSexpr]
; The actual commant function, invoked on a message event in the server.
; Takes in the Client that sent the message, and the payload of the message.
(struct command (name args desc proc))
