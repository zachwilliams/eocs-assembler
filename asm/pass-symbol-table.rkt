#lang racket

(require "asm-base.rkt"
         "asm-support.rkt")

(provide init-symbol-table
         add-labels-to-table
         rewrite-with-addresses
         add-memory-addresses-to-table
         )

;; Originally, the assembler was written in a purely
;; functional style. Truly, this would be better, for 
;; a variety of reasons. It was rewritten in an imperative
;; style (with mutation); as a result, the following
;; code is for manipulating the symbol table, which we 
;; represent with a hash table.
(define SYMBOL-TABLE (make-hash))

;; CONTRACT
;; init-symbol-table
;; PURPOSE
;; Loads default symbols into the table.
(define (init-symbol-table)
  (map (λ (pair)
         (table-add! (first pair) (second pair)))
       `((SP 0) (LCL 1) (ARG 2) (THIS 3) (THAT 4)
         ,@(map (λ (n)
                  (list (string->symbol (format "R~a" n)) n))
                (list 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))
         (SCREEN 16384) (KBD 24576))))

;; CONTRACT
;; table-add! :: symbol number -> void
;; PURPOSE
;; Adds a new label or memory address to the table.
;; Prevents the addition of the same label or address
;; multiple times.
(define (table-add! sym location)
  (if (in-table? sym)
      (error (format "Cannot add symbol '~a' to the lookup table more than once."
                     sym))
      (hash-set! SYMBOL-TABLE sym location)))

;; CONTRACT
;; table-lookup :: symbol -> number
;; PURPOSE
;; Returns the address associated with the given symbol.
(define (table-lookup label)
  (hash-ref SYMBOL-TABLE label 
            (λ () 
              (error (format "No label found for '~a'" label)))))

;; CONTRACT
;; in-table? :: symbol -> boolean
;; PURPOSE
;; Returns true if we find the symbol in the table,
;; false otherwise.
(define (in-table? label)
  (hash-ref SYMBOL-TABLE
            label 
            (λ () false)))


;; CONTRACT
;; add-labels-to-table :: (list-of instructions) address -> (list-of instructions)
;; PURPOSE
;; Walks through a list of instructions, adding labels to the 
;; lookup table. Increments the address as we go. Remember
;; that a label should not cause the address to go up, as 
;; labels will ultimately be removed. Return the list of instructions
;; as-is.
(define (add-labels-to-table loi)
  (cond
    [(empty? loi) '()]
    [(label? (first loi)) 
     (table-add! (label-name (first loi)) 
                 (label-addr (first loi)))
     (add-labels-to-table (rest loi))]
    [else (add-labels-to-table (rest loi))]))


;; CONTRACT
;; add-memory-addresses-to-table :: (list-of instructions) addr -> (l-o instr)
;; PURPOSE
;; Walks through the table. When we find a symbolic memory reference,
;; (for example, @i), place the variable name into the symbol table, in
;; addition to the address where it is stored. Remember, this will
;; be a function that processes A instructions... but only those A 
;; instructions that have a value field that is symbolic.
;;
;; Rebuild the list of instructions as you go.
(define (add-memory-addresses-to-table loi mem)
  (cond
    [(empty? loi) '()]
    [(A? (first loi)) 
     (cond
       [(in-table? (A-value (first loi)))
        (add-memory-addresses-to-table (rest loi) mem)]
       [(symbol? (A-value (first loi)))
        (table-add! (A-value (first loi)) mem) 
        (add-memory-addresses-to-table (rest loi) (+ 1 mem))]
       [else ;; the a instruction is a number
        (add-memory-addresses-to-table (rest loi) mem)])]
    [else ;c instruction or label
     (add-memory-addresses-to-table (rest loi) mem)]))

;; CONTRACT
;; rewrite-with-addresses :: (list-of instructions) -> (list-of instructions)
;; PURPOSE
;; Takes a list of instructions and rewrites the instruction stream
;; so that no symbolic references remain. 
(define (rewrite-with-addresses loi)
  (cond
    [(empty? loi) '()]
    [(label? (first loi))
     (rewrite-with-addresses (rest loi))]
    [(A? (first loi))
     (cond 
        [(in-table? (A-value (first loi))) 
         (cons 
          (A (A-addr (first loi)) (table-lookup (A-value (first loi))))
          (rewrite-with-addresses (rest loi)))] 
        [else ;; the a instruction is a number 
         (cons (first loi) (rewrite-with-addresses (rest loi)))])]
    [else ;c instruction or label  
     (cons (first loi) (rewrite-with-addresses (rest loi)))]))

;;;;;;;;;;;;;;;;;;;;;;
;;    for testing   ;;
;;;;;;;;;;;;;;;;;;;;;;
(define loi (list
   (label 0 'label0)
   (C 0 'D1 'C2 'J3)
   (A 1 0)
   (label 2 'label4)
   (label 2 'label5)
   (label 2 'label6)
   (label 2 'label7)
   (label 2 'label8)
   (C 2 'D9 'C10 'J11)
   (C 3 'D12 'C13 'J14)
   (A 4 'next)
   (C 5 'D15 'C16 'J17)
   (C 6 'D18 'C19 'J20)
   (A 7 'label5)
   (C 8 'D21 'C22 'J23)
   (A 9 22)
   (C 10 'D24 'C25 'J26)))


(add-labels-to-table loi)
(add-memory-addresses-to-table loi 10)
(rewrite-with-addresses loi)


