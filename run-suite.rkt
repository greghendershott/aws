#lang racket

(require rackunit
         rackunit/text-ui
         "tests/data.rkt")

(define-syntax-rule (define/run-test-suite name body0 body ...)
  (begin
    (printf "~a ... " name)
    (flush-output)
    (void (run-tests (test-suite name body0 body ...) 'normal))))

(provide (all-from-out rackunit)
         (all-from-out rackunit/text-ui)
         (all-from-out "tests/data.rkt")
         define/run-test-suite)
