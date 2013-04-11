#lang setup/infotab

(define name "aws")
(define categories '(net))
(define blurb
  '("Provides support for Amazon Web Services, including S3, SDB, SES, SNS, SQS, CloudWatch, Glacier Dynamo, Route 53."))
(define homepage "https://github.com/greghendershott/aws")

(define release-notes
  '((p "Use Racket's new define-logger.")))
(define version "2013-04-11")
(define can-be-loaded-with 'all)

(define primary-file '("main.rkt"))
(define scribblings '(("manual.scrbl" (multi-page))))

(define required-core-version "5.3.1")
(define repositories '("4.x"))
