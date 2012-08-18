#lang racket

(require xml
         (planet gh/http/request)
         (planet gh/http/head)
         "util.rkt"
         "keys.rkt"
         "exn.rkt"
         "post.rkt"
         )

(define sns-endpoint (make-parameter
                      (endpoint "sns.us-east-1.amazonaws.com" #f)))
(provide sns-endpoint)

(define/contract/provide (sns params [result-proc values])
  (((listof (list/c symbol? string?)))
   ((xexpr? . -> . list?)) . ->* .
   list?)
  (ensure-have-keys)
  (define common-params
    `((AWSAccessKeyId ,(public-key))
      (SignatureMethod "HmacSHA1")
      (SignatureVersion "2")
      (Timestamp ,(timestamp))
      (Version "2010-03-31")))
  (define all-params (sort (append params common-params)
                           (lambda (a b)
                             (string<? (symbol->string (car a))
                                       (symbol->string (car b))))))
  (define str-to-sign
    (string-append "GET" "\n"
                   (endpoint->host:port (sns-endpoint)) "\n"
                   "/" "\n"
                   (dict->form-urlencoded all-params)))
  (define signature (sha1-encode str-to-sign))
  (define signed-params (append all-params `((Signature ,signature))))
  (define qp (dict->form-urlencoded signed-params))
  (define uri (endpoint->uri (sns-endpoint) (string-append "/?" qp)))
  (define x
    (call/input-request
     "1.1" "GET" uri '()
     (lambda (in h)
       (define e (read-entity/xexpr in h))
       (match (extract-http-code h)
         [200 e]
         [else (raise (header&response->exn:fail:aws
                       h e (current-continuation-marks)))]))))
  (append (result-proc x)
          ;; If a NextToken element in the response XML, we need to
          ;; call again to get more values.
          (match (tags x 'NextToken)
            [(list `(NextToken () ,token))
             (sns (set-next-token params token)
                  result-proc)]
             [else '()])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define/contract/provide (create-topic name)
  (string? . -> . string?)
  (first (sns `((Action "CreateTopic")
                (Name ,name))
              (lambda (x) (map third (tags x 'TopicArn))))))

(define/contract/provide (delete-topic arn)
  (string? . -> . any)
  (void (sns `((Action "DeleteTopic")
               (TopicArn ,arn)))))

(define/contract/provide (get-topic-attributes arn)
  (string? . -> . (listof (cons/c symbol? string?)))
  (sns `((Action "GetTopicAttributes")
         (TopicArn ,arn))
       (lambda (x)
         (for/list ([x (in-list (tags x 'entry))])
             (match x
               [(list 'entry '() junk ...
                      (list 'key '() key)
                      junk ...
                      (list 'value '() val ...)
                      junk ...)
                (cons (string->symbol key) (string-join val ""))]
               [else (cons 'bad "val")])))))

(define/contract/provide (list-topics)
  (-> (listof string?))
  (sns `((Action "ListTopics"))
       (lambda (x) (map third (tags x 'TopicArn)))))

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
    [else #f]))
    
(define/contract/provide (subscribe endpoint protocol topic-arn)
  (string? sns-protocol? string? . -> . string?)
  (first (sns `((Action "Subscribe")
                (Endpoint ,endpoint)
                (Protocol ,protocol)
                (TopicArn ,topic-arn))
              (lambda (x)
                (list (first-tag-value x 'SubscriptionArn))))))

(define/contract/provide (unsubscribe subscription-arn)
  (string? . -> . any)
  (void (sns `((Action "Unsubscribe")
               (SubscriptionArn ,subscription-arn)))))

(define/contract/provide (publish arn msg #:subject [subject ""] #:json? [json? #f])
  ((string? string?)
   (#:subject string? #:json? boolean?)
   . ->* . any)
  (sns (append `((Action "Publish")
                 (Message ,msg)
                 (Subject ,subject)
                 (TopicArn ,arn))
                (if json?
                    `((MessageStructure "json"))
                    `()))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test

(module+ test
  (require rackunit
           "tests/data.rkt")

  (define (member? x xs)
    (not (not (member x xs))))

  (test-case
   "sns"
   (read-keys)
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
  (void))

 ;; Unfortunately it will be hard to write tests for most other SNS
 ;; functionality because they require a subscription to be confirmed
 ;; by the recipient.
