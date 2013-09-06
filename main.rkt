#lang racket

(require (for-syntax syntax/parse
                     racket/syntax
                     "core.rkt"))

(provide defn defn-)

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
