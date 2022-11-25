#lang info
(define license 'BSD-2-Clause)
(define version "1.15")
(define collection 'multi)
(define deps '(["base" #:version "6.3"]
               ["http" #:version "0.3"]
               "sha"))
(define build-deps '("at-exp-lib"
                     "racket-doc"
                     "rackunit-lib"
                     "scribble-lib"))
