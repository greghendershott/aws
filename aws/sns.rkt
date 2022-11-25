;; Copyright (c) 2012-2022 by Greg Hendershott.
;; SPDX-License-Identifier: BSD-2-Clause

#lang racket/base

(require http/head
         http/request
         racket/contract/base
         racket/dict
         racket/function
         racket/list
         racket/match
         racket/string
         xml/xexpr
         "exn.rkt"
         "keys.rkt"
         "post.rkt"
         "sigv4.rkt"
         "util.rkt")

(provide sns-endpoint
         sns-region)

(define sns-endpoint
  (make-parameter (endpoint "sns.us-east-1.amazonaws.com" #f)))
(define sns-region
  (make-parameter "us-east-1"))

(define/contract/provide (sns params [result-proc values])
  (->* ((listof (list/c symbol? string?)))
       ((xexpr/c . -> . list?))
       list?)
  (ensure-have-keys)
  (let* ([qp-text (dict->form-urlencoded (sort params param<?))]
         [uri (endpoint->uri (sns-endpoint) (string-append "/?" qp-text))]
         [heads (hasheq 'Host (endpoint-host (sns-endpoint))
                        'Date (seconds->gmt-8601-string 'basic (current-seconds))
                        'Content-Type "application/xml")]
         [heads (add-v4-auth-heads #:heads   heads
                                   #:method  "GET"
                                   #:uri     uri
                                   #:sha256  (sha256-hex-string #"")
                                   #:region  (sns-region)
                                   #:service "sns")]
         [result (call/input-request
                  "1.1" "GET" uri heads
                  (λ (in h)
                    (define e (read-entity/xexpr in h))
                    (match (extract-http-code h)
                      [200 e]
                      [_   (raise (header&response->exn:fail:aws
                                   h e (current-continuation-marks)))])))])
    (append (result-proc result)
            ;; If a NextToken element in the response XML, we need to
            ;; call again to get more values.
            (match (se-path*/elements '(NextToken) result)
              [(list `(NextToken () ,token)) (sns (set-next-token params token)
                                                  result-proc)]
              [_                             '()]))))

(define/contract/provide (create-topic name)
  (-> string? string?)
  (match (sns `((Action "CreateTopic")
                (Name ,name))
              (curry se-path*/list '(TopicArn)))
    [(cons arn _) arn]
    [_            (error 'create-topic "Unexpected response")]))

(define/contract/provide (delete-topic arn)
  (-> string? any)
  (void (sns `((Action "DeleteTopic")
               (TopicArn ,arn)))))

(define/contract/provide (get-topic-attributes arn)
  (-> string? (listof (cons/c symbol? string?)))
  (sns `((Action "GetTopicAttributes")
         (TopicArn ,arn))
       (λ (x)
         (for/list ([x (in-list (se-path*/elements '(entry) x))])
             (match x
               [(list 'entry '() _ ...
                      (list 'key '() key)
                      _ ...
                      (list 'value '() val ...)
                      _ ...)
                (cons (string->symbol key) (string-join val ""))]
               [_ (cons 'bad "val")])))))

(define/contract/provide (list-topics)
  (-> (listof string?))
  (sns `((Action "ListTopics"))
       (curry se-path*/list '(TopicArn))))

(struct subscription (owner topic-arn subscription-arn protocol endpoint)
        #:transparent)
(provide (struct-out subscription))

(define (xexpr->subscriptions x)
  (for/list ([x (in-list (se-path*/elements '(Subscriptions member) x))])
      (subscription (se-path* '(Owner) x)
                    (se-path* '(TopicArn) x)
                    (se-path* '(SubscriptionArn) x)
                    (se-path* '(Protocol) x)
                    (se-path* '(Endpoint) x))))

(define/contract/provide (list-subscriptions)
  (-> (listof subscription?))
  (sns `((Action "ListSubscriptions"))
       xexpr->subscriptions))

(define/contract/provide (list-subscriptions-by-topic arn)
  (-> string? (listof subscription?))
  (sns `((Action "ListSubscriptionsByTopic")
         (TopicArn ,arn))
       xexpr->subscriptions))

(define/provide (sns-protocol? s)
  (match s
    [(or "http"       ;delivery of JSON-encoded message via HTTP POST
         "https"      ;delivery of JSON-encoded message via HTTPS POST
         "email"      ;delivery of message via SMTP
         "email-json" ;delivery of JSON-encoded message via SMTP
         "sms"        ;delivery of message via SMS
         "sqs"        ;delivery of JSON-encoded message to an Amazon SQS queue
         )
     #t]
    [_ #f]))

(define/contract/provide (subscribe endpoint protocol topic-arn)
  (-> string? sns-protocol? string? string?)
  (match (sns `((Action "Subscribe")
                (Endpoint ,endpoint)
                (Protocol ,protocol)
                (TopicArn ,topic-arn))
              (λ (x)
                (list (se-path* '(SubscriptionArn) x))))
    [(cons arn _) arn]
    [_            (error 'create-topic "Unexpected response")]))

(define/contract/provide (unsubscribe subscription-arn)
  (-> string? any)
  (void (sns `((Action "Unsubscribe")
               (SubscriptionArn ,subscription-arn)))))

(define/contract/provide (publish arn msg
                                  #:subject [subject "No subject"]
                                  #:json? [json? #f])
  (->* (string? string?)
       (#:subject string? #:json? boolean?)
       any)
  (sns (append `((Action "Publish")
                 (Message ,msg)
                 (TopicArn ,arn)
                 (Subject ,subject))
                (if json?
                    `((MessageStructure "json"))
                    `()))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test

(module+ test
  (require rackunit
           "tests/data.rkt")
  (when (test-data-exists?)
    (define (member? x xs)
      (not (not (member x xs))))
    (define (do-test)
      (define arn (create-topic (test/topic)))
      (check-true (member? arn (list-topics)))
      (check-equal? (assoc 'TopicArn (get-topic-attributes arn))
                    (cons 'TopicArn arn))
      (check-equal? (subscribe (test/recipient) "email" arn)
                    "pending confirmation")
      (publish arn "Test" #:subject "Test")
      (publish arn "{\"default\": \"Test\"}" #:subject "Test" #:json? #t)
      (delete-topic arn)
      (check-false (member? arn (list-topics))))
    (read-keys)
    ;; Test a couple regions, especially eu-central-1 (Frankfurt)
    ;; which supports only v4 authentication.
    (for ([region '("us-east-1" "eu-central-1")])
      (parameterize ([sns-region region]
                     [sns-endpoint (endpoint (string-append "sns."
                                                            region
                                                            ".amazonaws.com")
                                             #f)])
        (do-test)))))

;; Unfortunately it will be hard to write tests for most other SNS
;; functionality because they require a subscription to be confirmed
;; by the recipient.
