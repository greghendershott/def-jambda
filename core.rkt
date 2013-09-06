#lang racket/base

(require syntax/parse
         racket/syntax
         racket/list
         racket/string
         racket/format)

;; Identifiers we compute in syntax templates must be `require`d
;; `for-template`. Otherwise, macro expansions will elicit errors
;; like, "module+: undefined` or "defproc: undefined". See
;; docs.racket-lang.org/syntax/Phases_and_Reusable_Syntax_Classes.html
(require (for-template (except-in racket -> case->)
                       (only-in typed/racket : -> case->)
                       (only-in scribble/manual defproc)))

(provide def)

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
   #:with (REQ-ARG-TYPES ...) (for/list ([req? (attribute req?s)]
                                         [type (attribute types)]
                                         #:when req?)
                                type)
   #:with (OPT-ARG-TYPES ...) (for/list ([req? (attribute req?s)]
                                         [type (attribute types)]
                                         #:unless req?)
                                type)
   #:with CONTRACT #'(->* (REQ-ARG-TYPES ...) (OPT-ARG-TYPES ...) RET-TYPE)
   #:with (CASE->S ...) (let ([stxs (syntax->list #'(OPT-ARG-TYPES ...))])
                          (for/list ([i (add1 (length stxs))])
                            #`(REQ-ARG-TYPES ... #,@(take stxs i) -> RET-TYPE)))
   #:with COLON #'(: ID (case-> CASE->S ...))
   #:with ((ARG-DECL ...) ...) #'(ARG.decl ...)
   #:with SIG #'(ID ARG-DECL ... ...)
   #:attr TEST #'(module+ test
                   (require rackunit)
                   (check-equal? (ID EX-ARGS ...) EX-RESULT) ...)
   #:with DOC #`(module+ doc
                  (defproc (ID ARG ...) RET-TYPE
                    DOC-STR ... "\n"
                    ;; TO-DO: Use actual `examples` form from Scribble:
                    #,(if (empty? (syntax->list #'(EX-RESULT ...)))
                          ""
                          "Examples:\n")
                    #,@(map (lambda (args res)
                              (format "> (~a ~a)\n~a\n"
                                      (syntax->datum #'ID)
                                      (string-join
                                       (map (compose ~a syntax->datum)
                                            (syntax->list args)))
                                      (syntax->datum res)))
                            (syntax->list #'((EX-ARGS ...) ...))
                            (syntax->list #'(EX-RESULT ...)))))))
