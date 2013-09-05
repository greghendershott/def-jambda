#lang racket

(require (for-syntax syntax/parse
                     racket/syntax
                     racket/list
                     racket/string
                     racket/function
                     racket/format))
(require scribble/manual)

(provide defn defn-
         defn/typed defn-/typed)

(define-syntax (defn stx)
  (core-defn stx
             (lambda (stx)
               (syntax-case stx ()
                 [(ID (ARG-DECL ...) (BODY ...) CONTRACT COLON TEST DOC)
                  #'(begin
                      (define (ID ARG-DECL ...)
                        BODY ...)
                      (provide (contract-out [ID CONTRACT]))
                      TEST
                      DOC)]))))

(define-syntax (defn- stx)
  (core-defn stx
             (lambda (stx)
               (syntax-case stx ()
                 [(ID (ARG-DECL ...) (BODY ...) CONTRACT COLON TEST DOC)
                  #'(begin
                      (define/contract (ID ARG-DECL ...)
                        CONTRACT
                        BODY ...)
                      TEST
                      DOC)]))))

(define-syntax (defn/typed stx)
  (core-defn stx
             (lambda (stx)
               (syntax-case stx ()
                 [(ID (ARG-DECL ...) (BODY ...) CONTRACT COLON TEST DOC)
                  #'(begin
                      COLON
                      (define (ID ARG-DECL ...)
                        BODY ...)
                      (provide ID))]))))

(define-syntax (defn-/typed stx)
  (core-defn stx
             (lambda (stx)
               (syntax-case stx ()
                 [(ID (ARG-DECL ...) (BODY ...) CONTRACT COLON TEST DOC)
                  #'(begin
                      COLON
                      (define (ID ARG-DECL ...)
                        BODY ...))]))))

;; Given stx for what is expected to be our function definition form,
;; and a function, calls the function with stx that is a list of the
;; interesting pattern variables we constructed. The function can then
;; `syntax-case` match to extract the pattern variables again and use
;; them in a template.  (Roughly speaking I suppose this is using a
;; syntax object like a dictionary or struct, and syntax-case like
;; dict-ref or a field access. Or more closely, like using
;; `(match-define (list ....) x)`. But with the difference that the
;; extacted things are pattern variables that may be used in a syntax
;; template.)
(define-for-syntax (core-defn stx f)
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
    [(defn (ID ARG:arg ... _:arrow RET-TYPE)
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
        [CONTRACT #'(->* (REQ-ARG-TYPES ...) (OPT-ARG-TYPES ...) RET-TYPE)]
        [COLON #'(: ID (REQ-ARG-TYPES ... -> RET-TYPE))] ;TO-DO: Opt & kw args
        [((ARG-DECL ...) ...) #'(ARG.decl ...)]
        [TEST #'(module+ test
                  (require rackunit)
                  (check-equal? (ID EX-ARGS ...) EX-RESULT) ...)]
        [DOC #`(module+ doc
                 (defproc (ID ARG ...) RET-TYPE
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
       (f #'(ID
             (ARG-DECL ... ...)
             (BODY ...)
             CONTRACT
             COLON
             TEST
             DOC
             )))]))
