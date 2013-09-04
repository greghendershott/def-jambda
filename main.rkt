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
  (core-defn stx #f #f))

(define-syntax (defn- stx)
  (core-defn stx #t #f))

(define-syntax (defn/typed stx)
  (core-defn stx #f #t))

(define-syntax (defn-/typed stx)
  (core-defn stx #t #t))

(define-for-syntax (core-defn stx private? typed?)
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
       (cond [typed?
              #`(begin
                  ;; Big TO-DO: Opt args and kw args
                  #'(: ID (REQ-ARG-TYPES ... -> RET-TYPE))
                  (define (ID ARG-DECL ... ...)
                    BODY ...)
                  #,@(cond [private? (list)]
                           [else (list #'(provide ID))]))]
             [else (cond [private? ;; Use define/contract, do not provide
                          #'(begin
                              (define/contract (ID ARG-DECL ... ...)
                                CONTRACT
                                BODY ...)
                              TEST
                              DOC)]
                         [else ;; Use define, and provide/contract
                          #'(begin
                              (define (ID ARG-DECL ... ...)
                                BODY ...)
                              (provide (contract-out [ID CONTRACT]))
                              TEST
                              DOC)])]))]))
