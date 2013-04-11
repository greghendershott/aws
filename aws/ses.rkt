#lang racket

(require net/base64
         net/url
         net/uri-codec
         xml
         http/request
         http/head
         "util.rkt"
         "keys.rkt"
         "exn.rkt"
         )

(define/contract/provide (send-email
                          #:from from
                          #:to to
                          #:subject subject
                          #:body body
                          #:cc [cc '()]
                          #:bcc [bcc '()]
                          #:reply-to [reply-to '()]
                          #:return-path [return-path from]
                          #:html? [html? #f]
                          #:charset [charset "UTF-8"])
  (( ;; required
    #:from string?
    #:to (listof string?)
    #:subject string?
    #:body string?
    )
   ( ;; optional
    #:cc (listof string?)
    #:bcc (listof string?)
    #:reply-to (listof string?)
    #:return-path string?
    #:html? boolean?
    #:charset string?
    ) . ->* . void)
  (request
   `((Action "SendEmail")
     (Source ,from)
     (ReturnPath ,return-path)
     (Message.Subject.Data ,subject)
     (,(if html? 'Message.Body.Html.Data 'Message.Body.Text.Data) ,body)
     (,(if html? 'Message.Body.Html.Charset 'Message.Body.Text.Charset) ,charset)
     ,@(addresses->heads "Destination.ToAddresses.member" to)
     ,@(addresses->heads "Destination.CcAddresses.member" cc)
     ,@(addresses->heads "Destination.BccAddresses.member" bcc)
     ,@(addresses->heads "ReplyToAddresses.member" reply-to)))
  (void))

(define/contract (addresses->heads prefix xs)
  (string? (listof string?) . -> . (listof (list/c symbol? string?)))
  (for/list ([x (in-list xs)]
             [n (in-naturals 1)])
      (list (string->symbol (format "~a.~a" prefix n)) x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define/contract/provide (send-raw-email mail-from rcpt-to raw-message)
  (string? (listof string?) string? . -> . xexpr?)
  (log-aws-debug raw-message)
  (define source ;use Return-Path if present else MAIL FROM
    (match raw-message
      [(regexp #rx"(?i:Return-Path:)[ ]*([^\r\n]+)" (list _ x)) x]
      [else mail-from]))
  (request
   `((Action "SendRawEmail")
     (Source ,source)
     ,@(addresses->heads "Destinations.member" rcpt-to)
     (RawMessage.Data ,(base64-encode/string raw-message)))))

(define (base64-encode/string s)
  (bytes->string/utf-8 (base64-encode (string->bytes/utf-8 s))))

(define/contract/provide (verify-email-address s)
  (string? . -> . void)
  (request `((Action "VerifyEmailAddress")
             (EmailAddress ,s)))
  (void))

(define/contract/provide (delete-verified-email-address s)
  (string? . -> . void)
  (request `((Action "DeleteVerifiedEmailAddress")
             (EmailAddress ,s)))
  (void))

(define/contract/provide (list-verified-email-addresses)
  (-> (listof string?))
  (define r (request `((Action "ListVerifiedEmailAddresses"))))
  (for/list ([x (in-list (tags r 'member))])
      (last x)))

(struct send-quota (sent-last-24-hours max-24-hour-send max-send-rate)
        #:transparent)
(provide (struct-out send-quota))
(define/contract/provide (get-send-quota)
  (-> send-quota?)
  (define r (request `((Action "GetSendQuota"))))
  (send-quota (num r 'SentLast24Hours)
              (num r 'Max24HourSend)
              (num r 'MaxSendRate)))

(struct send-statistic (time delivery-attempts rejects bounces complaints)
        #:transparent)
(provide (struct-out send-statistic))
(define/contract/provide (get-send-statistics)
  (-> (listof send-statistic?))
  (define r (request `((Action "GetSendStatistics"))))
  (for/list ([x (in-list (tags r 'member))])
      (send-statistic (str x 'Timestamp)
                      (num x 'DeliveryAttempts)
                      (num x 'Rejects)
                      (num x 'Bounces)
                      (num x 'Complaints))))

(define (num x t)
  (string->number (str x t)))
(define (str x t)
  (last (first (tags x t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ses-endpoint (make-parameter
                      (endpoint "email.us-east-1.amazonaws.com" #t)))
(provide ses-endpoint)

;; The low-level function used by other functions to make requests to
;; SES.
(define/contract/provide (request params)
  ((listof (list/c symbol? string?)) . -> . xexpr?)
  (ensure-have-keys)
  (define date-str (seconds->gmt-string))
  (define data (string->bytes/utf-8 (dict->form-urlencoded params)))
  (define heads
    (dict-set*
     (make-auth-header date-str)
     'Date date-str
     'Content-Type "application/x-www-form-urlencoded; charset=utf-8"))
  (call/output-request
   "1.1" "POST" (endpoint->uri (ses-endpoint) "/") data #f heads
   (lambda (in h)
     (define e (read-entity/xexpr in h))
     (match (extract-http-code h)
       [200 e]
       [else
        (define exn
          (header&response->exn:fail:aws h e (current-continuation-marks)))
        (match exn
          [(exn:fail:aws _ _ 400 _ "Throttling" "Maximum sending rate exceeded.")
           (define secs (add1 (* (random) 15))) ;1-16 seconds
           (log-aws-warning (format "~a. Will retry in ~a seconds." exn secs))
           (sleep secs)
           (request params)]
          [else (raise exn)])]))))

(define/contract (make-auth-header date-str)
  (string? . -> . dict?)
  ;; X-Amzn-Authorization: AWS3-HTTPS AWSAccessKeyId=<AccessKeyID>,\
  ;;   Algorithm=HmacSHA256,Signature=<Signature>
  (hash 'X-Amzn-Authorization
        (string-append "AWS3-HTTPS AWSAccessKeyId="
                       (public-key)
                       ",Algorithm=HmacSHA256,Signature="
                       (sha256-encode date-str))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test

(module+ test
  (require rackunit "tests/data.rkt")

  (define-syntax-rule (400-error? code message expr)
    (check-true
     (with-handlers
         ([exn:fail:aws?
           (lambda (exn)
             (match exn
               [(exn:fail:aws _ _ 400 "Bad Request" code msg) #t]
               [else #f]))])
       ;; We expect expr to raise an exception. Return #f if it doesn't
       (begin expr #f))))

  (test-case
   "miscellaneous"
   (check-true (list? (list-verified-email-addresses)))
   (check-true (send-quota? (get-send-quota)))
   (check-true (list? (get-send-statistics)))
   (send-email #:from (test/verified-sender)
               #:to (list (test/recipient))
               #:subject "test good address"
               #:body "test good address"))
  
  (test-case
   "400 errors"
   (400-error? "InvalidParameterValue"
               "Domain ends with dot"
               (send-email #:from (test/verified-sender)
                           #:to (list (string-append (test/recipient) "."))
                           #:subject "test bad address"
                           #:body "test bad address")))
  (400-error? "MessageRejected"
              "Email address is not verified."
              (send-email #:from (test/unverified-sender)
                          #:to (list (test/recipient))
                          #:subject "test unverified sender"
                          #:body "test unverified sender"))
  (void))
