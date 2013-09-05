#lang typed/racket

(require "typed/main.rkt")

(define (dbg stx)
  (pretty-print (syntax->datum (expand-once stx))))

(dbg
 #'(defn (mult [x Number][y Number] -> Number)
     (* x y)))

(defn (mult [x Number][y Number] -> Number)
     (* x y))
(mult 1 2)
