#lang racket

(require (for-syntax syntax/parse
                     racket/syntax))

(require (for-syntax "core.rkt"))

(provide defn defn-
         defn/typed defn-/typed)

(define-syntax (defn stx)
  (syntax-parse stx
    [d:def #`(begin
               (provide (contract-out [d.ID d.CONTRACT]))
               (define d.SIG d.BODY ...)
               d.TEST
               d.DOC)]))

(define-syntax (defn- stx)
  (syntax-parse stx
    [d:def (displayln (attribute d.TEST))
           (displayln (syntax-source (attribute d.TEST)))
           (displayln (syntax-source-module (attribute d.TEST) #t))
           #'(begin
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
