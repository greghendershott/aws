#lang setup/infotab
(define version "1.1")
(define collection 'multi)
(define deps '("base"
               ["http" "0.3"]
               "sha"
               "rackunit-lib"))
(define build-deps '("racket-doc"
                     "rackunit-lib"
                     "scribble-lib"))
