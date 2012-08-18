#lang setup/infotab

(define name "aws")
(define categories '(net))
(define blurb
  '("Provides support for some Amazon Web Services,"
    "such as S3, SDB, SES, SNS, and SQS."))
(define homepage "https://github.com/greghendershott/aws")

(define release-notes
  '((p "Fix formatting of code examples.")))
(define version "2012-08-18")
(define can-be-loaded-with 'all)

(define primary-file '("main.rkt"))
(define scribblings '(("manual.scrbl" (multi-page))))

(define required-core-version "5.3")
(define repositories '("4.x"))
