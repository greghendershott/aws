#lang setup/infotab

(define name "aws")
(define categories '(net))
(define blurb
  '("Provides support for Amazon Web Services, including"
    "S3, SDB, SES, SNS, SQS, CloudWatch, Glacier."))
(define homepage "https://github.com/greghendershott/aws")

(define release-notes
  '((p "Add support for the brand-new Amazon Glacier service.")))
(define version "2012-08-30")
(define can-be-loaded-with 'all)

(define primary-file '("main.rkt"))
(define scribblings '(("manual.scrbl" (multi-page))))

(define required-core-version "5.3")
(define repositories '("4.x"))
