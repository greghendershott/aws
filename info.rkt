#lang setup/infotab

(define name "aws")
(define categories '(net))
(define blurb
  '("Provides support for some Amazon Web Services,"
    "such as S3, SDB, SES, SNS, and SQS."))
(define release-notes
  '((p "Don't use evaluator for Scribble examples.")))
(define homepage "https://github.com/greghendershott/aws")

(define version "2012-08-18")
(define can-be-loaded-with 'all)

(define primary-file '("main.rkt"
                       "cw.rkt" "keys.rkt" "exn.rkt" "s3.rkt" "sdb.rkt"
                       "ses.rkt" "sns.rkt" "sqs.rkt" "util.rkt"))
(define scribblings '(("manual.scrbl" (multi-page))))

(define required-core-version "5.3")
(define repositories '("4.x"))
