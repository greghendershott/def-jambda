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

(begin-for-syntax
 (define-syntax-class arg
   #:description "function argument: [ maybe-#:kw id contract maybe-default ]"
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
 (define-syntax-class def
   (pattern
    (defn (ID ARG:arg ... _:arrow RET-TYPE)
      (~seq #:doc DOC-STR) ...
      (~seq #:ex [EX-ARGS ... _:arrow EX-RESULT]) ...
      BODY ...+)
    #:attr req?s (syntax->datum #'(ARG.req? ...))
    #:attr types (syntax->list  #'(ARG.type ...))
    #:with
    (REQ-ARG-TYPES ...)
    (for/list ([req? (attribute req?s)] [type (attribute types)] #:when req?)
      type)
    #:with
    (OPT-ARG-TYPES ...)
    (for/list ([req? (attribute req?s)] [type (attribute types)] #:unless req?)
      type)
    #:with
    CONTRACT #'(->* (REQ-ARG-TYPES ...) (OPT-ARG-TYPES ...) RET-TYPE)
    #:with
    COLON #'(: ID (REQ-ARG-TYPES ... -> RET-TYPE)) ;TO-DO: Opt & kw args
    #:with
    ((ARG-DECL ...) ...) #'(ARG.decl ...)
    #:with
    SIG #'(ID ARG-DECL ... ...)
    #:with
    TEST #'(module+ test
             (require rackunit)
             (check-equal? (ID EX-ARGS ...) EX-RESULT) ...)
    #:with
    DOC #`(module+ doc
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
                      (syntax->list #'(EX-RESULT ...))))))))

(define-syntax (defn stx)
  (syntax-parse stx
    [d:def #`(begin
               (provide (contract-out [d.ID d.CONTRACT]))
               (define d.SIG d.BODY ...)
               d.TEST
               d.DOC)]))

(define-syntax (defn- stx)
  (syntax-parse stx
    [d:def #'(begin
               (define/contract d.SIG d.CONTRACT d.BODY ...)
               d.TEST
               d.DOC)]))

(define-syntax (defn/typed stx)
  (syntax-parse stx
    [d:def #'(begin
               (provide d.ID)
               d.COLON
               (define d.SIG d.BODY ...)
               d.TEST
               d.DOC)]))

(define-syntax (defn-/typed stx)
  (syntax-parse stx
    [d:def #'(begin
               d.COLON
               (define d.SIG d.BODY ...)
               d.TEST
               d.DOC)]))
