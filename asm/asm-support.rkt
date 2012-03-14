#lang racket

(require "asm-base.rkt")

(provide extract-dest
         extract-comp
         extract-jump
 
         @inst->symbol
         @inst->number
         
         sym
         
         ;; Random bits
         ;; Exported for testing only.
         string->number-or-symbol
         )

;; CONTRACT
;; string->number-or-symbol :: string -> (U symbol? number?)
;; PURPOSE
;; If the string provided can be turned into a number,
;; then it is. Otherwise, it is turned into a symbol.
(define (string->number-or-symbol str)
  (if (string->number str)
      (string->number str)
      (string->symbol str)))

;; CONTRACT
;; @inst->number :: string -> number
;; PURPOSE
;; Takes an @ instruction and returns the number
;; that it contains.
(define (@inst->number asm)
  (string->number
   (second (regexp-match "([0-9]+)" asm))))

;; CONTRACT
;; @inst->number :: string -> number
;; PURPOSE
;; Takes an @ instruction and returns the symbol
;; that it contains.
(define (@inst->symbol asm)
  (string->symbol
   ;; It must start with alpha characters, and then anything can follow
   (second (regexp-match "([a-zA-Z]+[a-zA-Z0-9_$.]+)" asm))))


;; CONTRACT
;; sym :: (U string symbol) -> symbol
;; PURPOSE
;; Returns a new, uniquely numbered symbol using
;; the first argument as a base.
(define sym
  (let ([c 0])
    (lambda (id)
      (let ([newsym (string->symbol
                     (format "~a~a" id c))])
        (set! c (add1 c))
        newsym))))

;; CONTRACT
;; pad :: string number -> string
;; PURPOSE
;; Pads a string with leading zeros.
(define (pad str ln)
  (string-append (make-string 
                  (- ln (string-length str))
                  #\0) str))

;; CONTRACT
;; number->15bit :: number -> string
;; PURPOSE
;; Converts a number to a binary string,
;; making sure the number is less than 2^15.
(define (number->15bit v)
  (if (< v (expt 2 15))
      (let ([n (number->string v 2)])
        (if (< (string-length n) 15)
            (pad n 15)
            n))
      (error (format "~a is not less than ~a." 
                     v
                     (expt 2 15)))))


;; CONTRACT
;; extract-comp :: string -> symbol
;; PURPOSE
;; Takes an instruction of the form
;;   dest=comp;...
;; or
;;   comp
;; and returns a symbol representing the 'dest'.
(define (extract-comp exp)
  (cond 
    [(regexp-match? DCJ-REGEXP exp)
     (string->number-or-symbol (third (regexp-match DCJ-REGEXP exp)))]
    [(regexp-match? CJ-REGEXP exp)
     (string->number-or-symbol (second (regexp-match CJ-REGEXP exp)))]
    [(regexp-match? DC-REGEXP exp)
     (string->number-or-symbol (third (regexp-match DC-REGEXP exp)))]      
    [(regexp-match? C-REGEXP exp)
     (string->number-or-symbol (second (regexp-match C-REGEXP exp)))]
    ))

;; CONTRACT
;; extract-dest :: string -> symbol
;; PURPOSE
;; Takes an instruction of the form
;;   dest=...
;; and returns a symbol representing the 'dest'.
(define (extract-dest exp)
  (cond 
    [(regexp-match? DCJ-REGEXP exp)
     (string->number-or-symbol (second (regexp-match DCJ-REGEXP exp)))]
    [(regexp-match? DC-REGEXP exp)
     (string->number-or-symbol (second (regexp-match DC-REGEXP exp)))]      
    ))

;; CONTRACT
;; extract-jump :: string -> symbol
;; PURPOSE
;; Takes an instruction of the form
;;   dest=comp;jump
;; and returns a symbol representing the 'dest'.
(define (extract-jump exp)
  (cond 
    [(regexp-match? DCJ-REGEXP exp)
     (string->number-or-symbol (fourth (regexp-match DCJ-REGEXP exp)))]
    [(regexp-match? CJ-REGEXP exp)
     (string->number-or-symbol (third (regexp-match CJ-REGEXP exp)))]      
    ))