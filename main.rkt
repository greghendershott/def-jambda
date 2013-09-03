#lang at-exp racket
;; Note: The `at-exp` only for usage examples in this same file.

(require (for-syntax syntax/parse
                     racket/syntax
                     racket/list
                     racket/string
                     racket/function
                     racket/format
                     racket/pretty)) ;just for debugging
(require scribble/manual)

(provide defn defn-)

(define-syntax (defn stx)
  (core-defn stx #f))

(define-syntax (defn- stx)
  (core-defn stx #t))

(define-for-syntax (core-defn stx private?)
  (define-syntax-class arg
    #:description "function argument [ [#:keyword] id contract [default] ]"
    (pattern [(~seq id:id type:expr)]
             #:attr decl #'(id)
             #:with req? #t)
    (pattern [(~seq kw:keyword id:id type:expr)]
             #:attr decl #'(kw id)
             #:with req? #t)
    (pattern [(~seq id:id type:expr default:expr)]
             #:attr decl #'([id default])
             #:with req? #f)
    (pattern [(~seq kw:keyword id:id type:expr default:expr)]
             #:attr decl #'(kw [id default])
             #:with req? #f))
  (define-syntax-class arrow
    #:description "-> or =>"
    (pattern (~or (~datum ->) (~datum =>))))
  (syntax-parse stx
    [(defn (ID ARG:arg ... _:arrow RET)
       (~seq #:doc DOC-STR) ...
       (~seq #:ex [EX-ARGS ... _:arrow EX-RESULT]) ...
       BODY ...+)
     (define req?s (syntax->datum #'(ARG.req? ...)))
     (define types (syntax->list  #'(ARG.type ...)))
     (with-syntax*
       ([(REQ-ARG-TYPES ...)
         (for/list ([req? req?s] [type types] #:when req?)
           type)]
        [(OPT-ARG-TYPES ...)
         (for/list ([req? req?s] [type types] #:unless req?)
           type)]
        [CONTRACT #'(->* (REQ-ARG-TYPES ...) (OPT-ARG-TYPES ...) RET)]
        [((ARG-DECL ...) ...) #'(ARG.decl ...)]
        [TEST #'(module+ test
                  (require rackunit)
                  (check-equal? (ID EX-ARGS ...) EX-RESULT) ...)]
        [DOC #`(module+ doc
                 (defproc (ID ARG ...) RET
                   DOC-STR ... "\n"
                   #,(cond [(= 0 (length (syntax->list #'(EX-RESULT ...)))) ""]
                           [else "Examples:\n"])
                   #,@(map (lambda (args res)
                             (format "> (~a ~a)\n~a\n"
                                     (syntax->datum #'ID)
                                     (string-join
                                      (map (compose ~a syntax->datum)
                                           (syntax->list args)))
                                     (syntax->datum res)))
                           (syntax->list #'((EX-ARGS ...) ...))
                           (syntax->list #'(EX-RESULT ...))))) ])
       (cond [private?                  ;Use define/contract, do not provide
              #'(begin
                  (define/contract (ID ARG-DECL ... ...)
                    CONTRACT
                    BODY ...)
                  TEST
                  DOC)]
             [else                      ;Use define, and provide/contract
              #'(begin
                  (define (ID ARG-DECL ... ...)
                    BODY ...)
                  (provide (contract-out [ID CONTRACT]))
                  TEST
                  DOC)]))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Examples

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
     (add1 x)
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
