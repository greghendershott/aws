#lang racket

(require rackunit
         rackunit/text-ui
         "tests/data.rkt")

(define-syntax-rule (def/run-test-suite body0 body ...)
  (begin
    (define __FILE__ (syntax-source (quote-syntax body0)))
    (define basename (cadr (regexp-match #rx"/?([^/]+?)$" __FILE__)))
    (display basename)
    (display " ... ")
    (flush-output)
    (void (run-tests (test-suite basename body0 body ...) 'normal))))

(provide (all-from-out rackunit)
         (all-from-out rackunit/text-ui)
         (all-from-out "tests/data.rkt")
         def/run-test-suite)
