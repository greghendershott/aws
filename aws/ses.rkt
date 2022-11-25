;; Copyright (c) 2012-2022 by Greg Hendershott.
;; SPDX-License-Identifier: BSD-2-Clause

#lang racket/base

(require (for-syntax racket/base)
         http/head
         http/request
         net/base64
         racket/contract/base
         racket/contract/region
         racket/dict
         racket/list
         racket/match
         xml/xexpr
         "exn.rkt"
         "keys.rkt"
         "sigv4.rkt"
         "util.rkt")

(define ses-endpoint
  (make-parameter (endpoint "email.us-east-1.amazonaws.com" #t)))
(define ses-region
  (make-parameter "us-east-1"))

(provide (contract-out [ses-endpoint (parameter/c endpoint?)]
                       [ses-region (parameter/c string?)]))

;; The low-level function used by other functions to make requests
(define/contract/provide (request params)
  (-> (listof (list/c symbol? string?)) xexpr?)
  (ensure-have-keys)
  (let* ([uri (endpoint->uri (ses-endpoint) "/")]
         [date (seconds->gmt-8601-string 'basic (current-seconds))]
         [body (string->bytes/utf-8 (dict->form-urlencoded params))]
         [heads (hasheq 'Host (endpoint-host (ses-endpoint))
                        'Date date
                        'Content-Type "application/x-www-form-urlencoded; charset=utf-8")]
         [heads (add-v4-auth-heads #:heads   heads
                                   #:method  "POST"
                                   #:uri     uri
                                   #:sha256  (sha256-hex-string body)
                                   #:region  (ses-region)
                                   #:service "ses")])
    (call/output-request
     "1.1" "POST" uri body (bytes-length body) heads
     (λ (in h)
       (define e (read-entity/xexpr in h))
       (match (extract-http-code h)
         [200 e]
         [_
          (match (header&response->exn:fail:aws h e (current-continuation-marks))
            [(and exn
                  (exn:fail:aws _ _ 400 _ "Throttling" "Maximum sending rate exceeded."))
             (define secs (add1 (random 15)))
             (log-aws-warning (format "~a. Will retry in ~a seconds." exn secs))
             (sleep secs)
             (request params)]
            [exn (raise exn)])])))))

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
  (->*
   (#:from string?
    #:to (listof string?)
    #:subject string?
    #:body string?)
   (#:cc (listof string?)
    #:bcc (listof string?)
    #:reply-to (listof string?)
    #:return-path string?
    #:html? boolean?
    #:charset string?)
   void)
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
  (-> string? (listof string?) (listof (list/c symbol? string?)))
  (for/list ([x (in-list xs)]
             [n (in-naturals 1)])
      (list (string->symbol (format "~a.~a" prefix n)) x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define/contract/provide (send-raw-email mail-from rcpt-to raw-message)
  (-> string? (listof string?) string? xexpr?)
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
  (-> string? void)
  (request `((Action "VerifyEmailAddress")
             (EmailAddress ,s)))
  (void))

(define/contract/provide (delete-verified-email-address s)
  (-> string? void)
  (request `((Action "DeleteVerifiedEmailAddress")
             (EmailAddress ,s)))
  (void))

(define/contract/provide (list-verified-email-addresses)
  (-> (listof string?))
  (se-path*/list '(VerifiedEmailAddresses member)
                 (request `((Action "ListVerifiedEmailAddresses")))))

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
  (for/list ([x (in-list (se-path*/elements '(member) r))])
      (send-statistic (str x 'Timestamp)
                      (num x 'DeliveryAttempts)
                      (num x 'Rejects)
                      (num x 'Bounces)
                      (num x 'Complaints))))

(define (num x t)
  (string->number (str x t)))
(define (str x t)
  (se-path* (list t) x))

(module+ test
  (require rackunit
           "tests/data.rkt")

  (when (test-data-exists?)
    (test-case
     "miscellaneous"
     (check-true (list? (list-verified-email-addresses)))
     (check-true (send-quota? (get-send-quota)))
     (check-true (list? (get-send-statistics)))
     (send-email #:from (test/verified-sender)
                 #:to (list (test/recipient))
                 #:subject "test good address"
                 #:body "test good address"))

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
    (void)))
