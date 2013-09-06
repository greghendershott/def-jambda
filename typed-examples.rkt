#lang typed/racket

(require "typed/main.rkt")

(define (dbg stx)
  (pretty-print (syntax->datum (expand-once stx))))

(dbg
 #'(defn (mult [x Number][y Number 1][z Any #f] -> Number)
     (* x y)))

;; Unfortunately this:

;; (defn (mult [x Number][y Number 1] -> Number)
;;     (* x y))
;; (mult 1 2)

;; gives an error:

; typed-examples.rkt:13:7: Type Checker: Expected Number, but got Any
;  in: x
; typed-examples.rkt:13:9: Type Checker: Expected Number, but got Any
;  in: y
; typed-examples.rkt:13:7: Type Checker: Expected Number, but got Any
;  in: x
; Type Checker: Summary: 3 errors encountered

;; even though:
;;
;; 1. It works without the optional argument.
;;
;; 2. It *appears* to expand to exactly the following hand-written
;; code, which *does* work fine:
;;
;;
;; (: mult (case-> [Number -> Number]
;;                 [Number Number -> Number]))
;; (define (mult x [y 1])
;;   (* x y))

;; WAT ??
