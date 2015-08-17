#lang racket/base

(require http/request
         racket/contract/base
         racket/dict
         racket/list
         racket/match
         xml/xexpr
         "keys.rkt"
         "post.rkt"
         "sigv4.rkt"
         "util.rkt")

(define sqs-endpoint
  (make-parameter (endpoint "sqs.us-east-1.amazonaws.com" #f)))
(define sqs-region
  (make-parameter "us-east-1"))

(provide (contract-out [sqs-endpoint (parameter/c endpoint?)]
                       [sqs-region (parameter/c string?)]))

(define/contract/provide (sqs uri params [result-proc values])
  (->* (string?
        (listof (list/c symbol? string?)))
       ((-> xexpr? list?))
       list?)
  (ensure-have-keys)
  (let* ([date (seconds->gmt-8601-string 'basic (current-seconds))]
         [params (append params
                         `((AWSAccessKeyId ,(public-key))
                           (SignatureMethod "HmacSHA256")
                           (SignatureVersion "4")
                           (Timestamp ,date)
                           (Version "2011-10-01")))]
         [body (string->bytes/utf-8 (dict->form-urlencoded params))]
         [heads (hasheq 'Host (endpoint-host (sqs-endpoint))
                        'Date date
                        'Content-Type "application/x-www-form-urlencoded; charset=utf-8")]
         [heads (dict-set* heads
                           "Authorization"
                           (aws-v4-authorization
                            "POST"
                            uri
                            heads
                            (sha256-hex-string body)
                            (sqs-region)
                            "sqs"))]
         [x (post-with-retry uri params heads)])
    (append (result-proc x)
            ;; If a NextToken element in the response XML, we need to
            ;; call again to get more values.
            (match (tags x 'NextToken)
              [(list `(NextToken () ,token))
               (sqs uri
                    (set-next-token params token)
                    result-proc)]
              [_ '()]))))

(define/contract/provide (create-queue name)
  (-> string? string?)
  (first-tag-value (sqs (endpoint->uri (sqs-endpoint) "/")
                        `((Action "CreateQueue")
                          (QueueName ,name)))
                   'QueueUrl))

(define/contract/provide (delete-queue q-uri)
  (-> string? void?)
  (void (sqs q-uri
             `((Action "DeleteQueue")))))

(define/contract/provide (list-queues)
  (-> (listof string?))
  (sqs (endpoint->uri (sqs-endpoint) "/")
       `((Action "ListQueues"))
       (lambda (x) (map third (tags x 'QueueUrl)))))

(define/contract/provide (get-queue-uri name)
  (-> string? string?)
  (first-tag-value (sqs (endpoint->uri (sqs-endpoint) "/")
                        `((Action "GetQueueUrl")
                          (QueueName ,name)))
                   'QueueUrl))

(define/contract/provide (send-message q-uri body [delay-seconds #f])
  ((string? string?) ((or/c #f exact-nonnegative-integer?)) . ->* . void?)
  (void (sqs q-uri
             `((Action "SendMessage")
               (MessageBody ,body)
               ,@(if delay-seconds
                     `((DelaySeconds ,(number->string delay-seconds)))
                     `())))))

(struct message (body md5 id receipt-handle attributes) #:transparent)

(define/contract/provide (receive-messages q-uri
                                           [max 1]
                                           [visibility-timeout #f])
  ((string?)
   ((and/c exact-integer? (between/c 1 10))
    (or/c #f exact-nonnegative-integer?)
    )
   . ->* . (listof message?))
  (sqs q-uri
       `((Action "ReceiveMessage")
         (AttributeName.1 "All")
         (MaxNumberOfMessages ,(number->string max))
         ,@(if visibility-timeout
               `((VisibilityTimeout ,(number->string visibility-timeout)))
               '()))
       (lambda (x)
         (for/list ([x (in-list (tags x 'Message))])
             (message (first-tag-value x 'Body)
                      (first-tag-value x 'MD5OfBody)
                      (first-tag-value x 'MessageId)
                      (first-tag-value x 'ReceiptHandle)
                      (map attribute-xexpr->attrib-pair (tags x 'Attribute)))))))


(define/contract/provide (receive-message q-uri [visibility-timeout #f])
  ((string?) ((or/c #f exact-nonnegative-integer?)) . ->* . message?)
  (define xsm (receive-messages q-uri 1 visibility-timeout))
  (unless (not (empty? xsm))
    (error 'receive-message "no messages returned"))
  (first xsm))

(define/contract/provide (delete-message q-uri receipt-handle)
  (-> string? string? void?)
  (void (sqs q-uri
             `((Action "DeleteMessage")
               (ReceiptHandle ,receipt-handle)))))

(define/contract/provide (get-queue-attributes q-uri)
  (-> string? (listof (list/c symbol? string?)))
  (sqs q-uri
       '((Action "GetQueueAttributes")
         (AttributeName.1 "All"))
       (lambda (x)
         (map attribute-xexpr->attrib-pair (tags x 'Attribute)))))

(define/contract/provide (change-message-visibility q-uri receipt-handle timeout)
  (-> string? string? exact-nonnegative-integer? void?)
  (void (sqs q-uri
             `((Action "ChangeMessageVisibility")
               (ReceiptHandle ,receipt-handle)
               (VisibilityTimeout ,(number->string timeout))))))

(module+ test
  (require rackunit
           "tests/data.rkt")
  (when (test-data-exists?)
    (test-case
     "sqs"
     (read-keys)
     (define q-uri (create-queue (test/queue)))
     (regexp-match? (regexp (string-append "/" q-uri "$")) q-uri)
     (list-queues)
     (check-equal? (get-queue-uri (test/queue)) q-uri)
     (get-queue-attributes q-uri)
     (define msg-body "Hello, world.")
     (send-message q-uri msg-body)
     (sleep 10.0) ;may take awhile for this to become available
     (define xsm (receive-messages q-uri 1))
     (check-true (not (empty? xsm)))
     (define m (first xsm))
     (check-equal? (message-body m) msg-body)
     (define rh (message-receipt-handle m))
     (change-message-visibility q-uri rh 10)
     (delete-message q-uri rh)
     ;; SQS will fail this if you delete a queue more than once < 60 seconds
     ;; So if you re-run this test too quickly, it may fail for that reason.
     (delete-queue q-uri))
    (void)))
