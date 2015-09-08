#lang setup/infotab
(define version "1.5")
(define collection 'multi)
(define deps '(["racket" "6.0.1"]
               "base"
               ["http" "0.3"]
               "sha"
               "rackunit-lib"))
(define build-deps '("racket-doc"
                     "rackunit-lib"
                     "scribble-lib"))
