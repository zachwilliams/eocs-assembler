#lang racket

(provide (all-defined-out))

;; Structures
(struct A (addr value) 
  #:inspector (make-inspector))

(struct C (addr dest comp jump)
  #:inspector (make-inspector))

(struct label (addr name) 
  #:inspector (make-inspector))

;; Regular expressions
(define DCJ-REGEXP #rx"(.+)=(.+);(.+)")
(define CJ-REGEXP #rx"(.+);(.+)")
(define DC-REGEXP #rx"(.+)=(.+)")
(define C-REGEXP #rx"(.+)")
(define ANUM-REGEXP #rx"@([0-9]+)")
(define ASYM-REGEXP #rx"@([A-Za-z]+[0-9]*[A-Za-z]*)")
(define LABEL-REGEXP #rx"\\((.+)\\)")

;; Macros
(define-syntax (each-with stx)
  (syntax-case stx ()
    [(each-with var body* ...)
     #`(let ([var (void)])
         (set! var body*) ...)]))

(define-syntax (each stx)
  (syntax-case stx ()
    [(each [dest body] ...)
     #`(let ([dest (void)] ...)
         (set! dest body) ...
         (first (reverse (list dest ...)))
         )
     ]))