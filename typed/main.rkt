#lang racket

(require (for-syntax syntax/parse
                     racket/syntax
                     "../core.rkt"))

(provide defn defn-)

(define-syntax (defn stx)
  (syntax-parse stx
    [d:def #'(begin
               (provide d.ID)
               d.COLON
               (define d.SIG d.BODY ...)
               d.TEST
               d.DOC)]))

(define-syntax (defn- stx)
  (syntax-parse stx
    [d:def #'(begin
               d.COLON
               (define d.SIG d.BODY ...)
               d.TEST
               d.DOC)]))
