#lang at-exp racket

(require "main.rkt")

(define (dbg stx)
  (pretty-print (syntax->datum (expand-once stx))))

(dbg
 #'(defn (foo [x number?]
              [y number? 0]
              [#:kw0 kw0 number?]
              [#:kw1 kw1 number? 0]
              -> number?)
     #:doc "Hi"
     #:ex [0 1 2 3 => 0]
     #:ex ["foo" 1 2 3 => (exn:fail:contract)]
     (* 2 x)))

(dbg
 #'(defn- (foo [x number?]
               [y number? 0]
               [#:kw0 kw0 number?]
               [#:kw1 kw1 number? 0]
               -> number?)
     #:doc @list{I am a doc string
                 On multiple lines}
     #:ex [0 1 2 3 => 0]
     #:ex [1 1 2 3 => 2]
     (* 2 x)))

(dbg
 #'(defn (foo [x number?] -> number?)
     (* 2 x)))

(dbg
 #'(defn (mult [x number?][y number? 1] -> number?)
  #:doc @list{Multiplies @racket[x] by @racket[y].
              Use it fruitfully.}
  #:ex [10 3 => 30]
  #:ex [0 3 => 0]
  #:ex [10 => 10]
  #:ex [0 => 0]
  ;; And the function body:
  (* x 3)))

(defn- (mult [x number?]
             [y number? 0]
             -> number?)
  #:doc "hi"
  #:ex [0 => 0]
  #:ex [10 20 => 200]
  (* x y))
(mult 10)
(mult 10 10)
;;(mult "10" 10)

;; To see the Scribble defproc result, at the REPL:
;; (require (submod "." doc))
