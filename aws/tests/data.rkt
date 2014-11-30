#lang racket/base

(require (for-syntax racket/base)
         racket/file
         racket/match)

(provide test-data-exists?)

;; Load personally identifying data required by tests from a file so
;; that it's not part of the repo and so that users may customize.
;;
;; This includes things like AWS names for buckets and domains, email
;; addresses to test using SES, and so on.
;;
;; We provide a function to call to get each piece of test data.  The
;; data will be loaded from file the first time such a call is made.
;; If no such calls are made, the file won't be loaded, meaning this
;; will work fine when used in production and not running unit tests.

(define data-file (build-path (find-system-path 'home-dir)
                              ".aws-tests-data"))

(define (test-data-exists?)
  (file-exists? data-file))

(define-syntax (def/prov stx)
  (syntax-case stx ()
      [(_ name)
       (with-syntax ([name/str #'(symbol->string 'name)])
         #'(begin
             (define (name)
               (get-test-data name/str))
             (provide name)))]))

(define (load-test-data-from-file)
  (for/hash ([x (in-list (file->lines data-file
                                      #:mode 'text
                                      #:line-mode 'any))])
      (match x
        [(pregexp "^\\s*([^#]+?)\\s*=\\s*([^#]+?)$" (list _ k v))
         (values k v)]
        [else
         (values 0 0)])))

(define get-test-data
  (let ([h #f]) ;don't try to load file unless we are actually called
    (lambda (name)
      (unless h
        (set! h (load-test-data-from-file)))
      (hash-ref h name
                (lambda ()
                  (error name
                         "not defined in ~/.aws-run-tests-data"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; used by s3.rkt

(def/prov test/bucket) ;do NOT use leading /
(def/prov test/path)   ;DO use leading /

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; used by ses.rkt

;; You must set this to an email address verified with SES, for
;; example using verify-email-address.
(def/prov test/verified-sender)

;; This should be an address you have NOT verified.
(def/prov test/unverified-sender)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; used by ses.rkt and sns.rkt

;; Set this to some email address at which you don't mind getting some
;; test emails.
(def/prov test/recipient)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; used by sdb.rkt

;; This domain will get munged -- data added/deleted, and in fact the
;; entire domain will be created/deleted -- so pick an SDB domain name
;; you're not using in production!
(def/prov test/domain)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; used by sns.rkt

;; This topic will get munged -- data added/deleted, and in fact the
;; entire topic will be created/deleted -- so pick an SDB topic name
;; you're not using in production!
(def/prov test/topic)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; used by sqs.rkt

;; This queue will get munged -- messages added/removed, and in fact
;; the entire queue will be created/deleted -- so pick an SQS queue
;; name you're not using in production!
(def/prov test/queue)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; used by cw.rkt

(def/prov test/namespace)
(def/prov test/metric)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; used by glacier.rkt

(def/prov test/vault)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; used by dynamo.rkt

(def/prov test/dynamo-table)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; used by r53.rkt

(def/prov test/domain.com)
