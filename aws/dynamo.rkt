#lang racket/base

(require (for-syntax racket/base)
         http/request
         json
         racket/contract/base
         racket/contract/region
         racket/dict
         "exn.rkt"
         "sigv4.rkt"
         "util.rkt")

(provide dynamo-endpoint
         dynamo-region
         attribute-type/c
         create-table
         delete-table
         describe-table
         list-tables
         batch-get-item
         batch-write-item
         get-item
         delete-item
         put-item
         query
         scan
         update-item
         update-table)

(define dynamo-endpoint
  (make-parameter (endpoint "dynamodb.us-east-1.amazonaws.com" #t)))
(define dynamo-region
  (make-parameter "us-east-1"))
(define service "dynamodb")

(define attribute-type/c (or/c "S" "N" "B"))

(define/contract (date+authorize method uri heads body)
  (string? string? dict? bytes? . -> . dict?)
  (let ([heads (dict-set* heads
                          'Host (endpoint-host (dynamo-endpoint))
                          'Date (seconds->gmt-8601-string 'basic
                                                          (current-seconds))
                          'Content-Type "application/x-amz-json-1.0")])
    (dict-set* heads
               "Authorization"
               (aws-v4-authorization
                method
                uri
                heads
                (sha256-hex-string body)
                (dynamo-region)
                service))))

;; All of the Dynamo functions POST JSON to /. The only variation is
;; the JSON content and the `x-amz-target` header.
(define/contract (raw js target)
  (jsexpr? string? . -> . jsexpr?)
  (define p "/")
  (define u (endpoint->uri (dynamo-endpoint) p))
  (define bstr (string->bytes/utf-8 (jsexpr->string js)))
  (define h (date+authorize "POST"
                            u
                            (hash 'x-amz-target target)
                            bstr))
  (call/output-request "1.1" "POST" u bstr (bytes-length bstr) h
                       (λ (in h)
                         (check-response in h)
                         (bytes->jsexpr (read-entity/bytes in h)))))

(define/contract (create-table name
                               read-units
                               write-units
                               hash-key-name
                               hash-key-type
                               [range-key-name #f]
                               [range-key-type #f])
  ((string?
    exact-positive-integer? exact-positive-integer?
    string? attribute-type/c)
   (string? attribute-type/c)
   . ->* .
   jsexpr?)
  (raw (hasheq
        'TableName name
        'KeySchema (apply hasheq
                          (append
                           (list 'HashKeyElement
                                 (hasheq 'AttributeName hash-key-name
                                         'AttributeType hash-key-type))
                           (if (and range-key-name range-key-type)
                               (list 'RangeKeyElement
                                     (hasheq 'AttributeName range-key-name
                                             'AttributeType range-key-type))
                               '())))
        'ProvisionedThroughput (hasheq 'ReadCapacityUnits read-units
                                       'WriteCapacityUnits write-units))
       "DynamoDB_20111205.CreateTable"))

(define/contract (delete-table name)
  (string? . -> . jsexpr?)
  (raw (hasheq 'TableName name)
       "DynamoDB_20111205.DeleteTable"))

(define/contract (describe-table name)
  (string? . -> . jsexpr?)
  (raw (hasheq 'TableName name)
       "DynamoDB_20111205.DescribeTable"))

(define/contract (list-tables #:limit [limit #f] #:from [from #f])
  (() (#:limit exact-positive-integer? #:from string?) . ->* . jsexpr?)
  (raw (apply hasheq
              (append (if limit (list 'Limit limit) '())
                      (if from (list 'ExclusiveStartTableName from) '())))
        "DynamoDB_20111205.ListTables"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The remainder, I think there's little value-add to hiding/wrapping
;; the JSON. Better to let people use it directly, consulting Amazon
;; docs?

(define-for-syntax (camelCase s)
  (let ([s (symbol->string (syntax->datum s))])
    (apply string-append (for/list ([s (regexp-split "-" s)])
                           (string-append (string-upcase (substring s 0 1))
                                        (substring s 1))))))

(define-syntax (defraw stx)
  (syntax-case stx ()
    [(_ name)
     (identifier? #'name)
     (with-syntax ([amz-target (string-append "DynamoDB_20111205."
                                              (camelCase #'name))])
       #'(define/contract (name js)
           (jsexpr? . -> . jsexpr?)
           (raw js amz-target)))]))

(defraw put-item)
(defraw get-item)
(defraw delete-item)
(defraw update-item)
(defraw batch-get-item)
(defraw batch-write-item)
(defraw query)
(defraw scan)
(defraw update-table)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ test
  (require rackunit
           "tests/data.rkt")
  (when (test-data-exists?)
    (test-case
     "dynamo"
     (define test (test/dynamo-table))
     (check-not-exn (λ () (create-table test 1 1
                                             "HashKey" "S"
                                             "RangeKey" "S")))
     (check-not-exn (λ () (list-tables #:limit 10)))
     (check-not-exn (λ () (describe-table test)))
     (let loop ()
       (define x (describe-table test))
       (define status (hash-ref (hash-ref x 'Table) 'TableStatus #f))
       (unless (equal? status "ACTIVE")
         (printf "Table status is '~a', waiting for 'ACTIVE'...\n" status)
         (sleep 15)
         (loop)))
     (check-not-exn
      (λ () (put-item (hasheq 'TableName test
                                   'Item (hasheq 'HashKey (hasheq 'S "Hi")
                                                 'RangeKey (hasheq 'S "world")
                                                 'Foo (hasheq 'S "bar"))))))
     (sleep 2)
     (define js
       (get-item (hasheq 'TableName test
                         'Key (hasheq 'HashKeyElement (hasheq 'S "Hi")
                                      'RangeKeyElement (hasheq 'S "world")))))
     (check-equal? (hash-ref (hash-ref js 'Item) 'Foo) (hasheq 'S "bar"))
     (check-not-exn (λ () (delete-table test))))))
