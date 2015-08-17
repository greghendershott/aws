#lang racket/base

(require http/head
         http/request
         racket/contract/base
         racket/dict
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
  (((listof (list/c symbol? string?))) ((xexpr? . -> . list?)) . ->* . list?)
  (ensure-have-keys)
  (let* ([qp-text (dict->form-urlencoded (sort params param<?))]
         [uri (endpoint->uri (sns-endpoint) (string-append "/?" qp-text))]
         [heads (hasheq 'Host (endpoint-host (sns-endpoint))
                        'Date (seconds->gmt-8601-string 'basic (current-seconds))
                        'Content-Type "application/xml")]
         [heads (dict-set* heads
                           'Authorization
                           (aws-v4-authorization "GET"
                                                 uri
                                                 heads
                                                 (sha256-hex-string #"")
                                                 (sns-region)
                                                 "sns"))]
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
            (match (tags result 'NextToken)
              [(list `(NextToken () ,token)) (sns (set-next-token params token)
                                                  result-proc)]
              [_                             '()]))))

(define/contract/provide (create-topic name)
  (string? . -> . string?)
  (match (sns `((Action "CreateTopic")
                (Name ,name))
              (λ (x) (map third (tags x 'TopicArn))))
    [(cons arn _) arn]
    [_            (error 'create-topic "Unexpected response")]))

(define/contract/provide (delete-topic arn)
  (string? . -> . any)
  (void (sns `((Action "DeleteTopic")
               (TopicArn ,arn)))))

(define/contract/provide (get-topic-attributes arn)
  (string? . -> . (listof (cons/c symbol? string?)))
  (sns `((Action "GetTopicAttributes")
         (TopicArn ,arn))
       (λ (x)
         (for/list ([x (in-list (tags x 'entry))])
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
       (λ (x) (map third (tags x 'TopicArn)))))

(struct subscription (owner topic-arn subscription-arn protocol endpoint)
        #:transparent)
(provide (struct-out subscription))

(define (xexpr->subscriptions x)
  (for/list ([x (in-list (tags x 'member))])
      (subscription (first-tag-value x 'Owner)
                    (first-tag-value x 'TopicArn)
                    (first-tag-value x 'SubscriptionArn)
                    (first-tag-value x 'Protocol)
                    (first-tag-value x 'Endpoint))))

(define/contract/provide (list-subscriptions)
  (-> (listof subscription?))
  (sns `((Action "ListSubscriptions"))
       xexpr->subscriptions))

(define/contract/provide (list-subscriptions-by-topic arn)
  (string? . -> . (listof subscription?))
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
  (string? sns-protocol? string? . -> . string?)
  (match (sns `((Action "Subscribe")
                (Endpoint ,endpoint)
                (Protocol ,protocol)
                (TopicArn ,topic-arn))
              (λ (x)
                (list (first-tag-value x 'SubscriptionArn))))
    [(cons arn _) arn]
    [_            (error 'create-topic "Unexpected response")]))

(define/contract/provide (unsubscribe subscription-arn)
  (string? . -> . any)
  (void (sns `((Action "Unsubscribe")
               (SubscriptionArn ,subscription-arn)))))

(define/contract/provide (publish arn msg
                                  #:subject [subject "No subject"]
                                  #:json? [json? #f])
  ((string? string?)
   (#:subject string? #:json? boolean?)
   . ->* . any)
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
