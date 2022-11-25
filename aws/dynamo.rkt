;; Copyright (c) 2012-2022 by Greg Hendershott.
;; SPDX-License-Identifier: BSD-2-Clause

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
         dynamo-api-version
         dynamo-request
         ;; The remainder are deprecated
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

(define dynamo-api-version
  (make-parameter "20111205")) ;for backward compatibility
(define (target op-name)
  (string-append "DynamoDB_" (dynamo-api-version) "." op-name))
(define (assert-20111205 who)
  (unless (equal? (dynamo-api-version) "20111205")
    (error who "only for backward compatibility when dynamo-api-version is \"20111205\"")))

(define attribute-type/c (or/c "S" "N" "B"))

(define/contract (date+authorize method uri heads body)
  (-> string? string? dict? bytes? dict?)
  (let ([heads (dict-set* heads
                          'Host (endpoint-host (dynamo-endpoint))
                          'Date (seconds->gmt-8601-string 'basic
                                                          (current-seconds))
                          'Content-Type "application/x-amz-json-1.0")])
    (add-v4-auth-heads #:heads heads
                       #:method method
                       #:uri uri
                       #:sha256 (sha256-hex-string body)
                       #:region (dynamo-region)
                       #:service service)))

;; All of the Dynamo functions POST JSON to /. The only variation is
;; the JSON content and the `x-amz-target` header.
(define/contract (dynamo-request op js)
  (-> string? jsexpr? jsexpr?)
  (define p "/")
  (define u (endpoint->uri (dynamo-endpoint) p))
  (define bstr (string->bytes/utf-8 (jsexpr->string js)))
  (define h (date+authorize "POST"
                            u
                            (hash 'x-amz-target (target op))
                            bstr))
  (call/output-request "1.1" "POST" u bstr (bytes-length bstr) h
                       (λ (in h)
                         (check-response in h)
                         (bytes->jsexpr (read-entity/bytes in h)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The remainder of this is only for backward compatibility when using
;; the deprecated DynamoDB version 20111205 API. For newer versions,
;; the intended use is for people to form the jsexpr themselves and
;; give to `dynamo-request` -- _not_ to use these wrappers.

(define/contract (create-table name
                               read-units
                               write-units
                               hash-key-name
                               hash-key-type
                               [range-key-name #f]
                               [range-key-type #f])
  (->*
   (string?
    exact-positive-integer? exact-positive-integer?
    string? attribute-type/c)
   (string? attribute-type/c)
   jsexpr?)
  (assert-20111205 'create-table)
  (dynamo-request
   "CreateTable"
   (hasheq
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
                                   'WriteCapacityUnits write-units))))

(define/contract (delete-table name)
  (-> string? jsexpr?)
  (assert-20111205 'delete-table)
  (dynamo-request "DeleteTable"
                  (hasheq 'TableName name)))

(define/contract (describe-table name)
  (-> string? jsexpr?)
  (assert-20111205 'describe-table)
  (dynamo-request "DescribeTable"
                  (hasheq 'TableName name)))

(define/contract (list-tables #:limit [limit #f] #:from [from #f])
  (->* () (#:limit exact-positive-integer? #:from string?) jsexpr?)
  (assert-20111205 'list-tables)
  (dynamo-request "ListTables"
                  (apply hasheq
                         (append (if limit (list 'Limit limit) '())
                                 (if from (list 'ExclusiveStartTableName from) '())))))

(define-for-syntax (camelCase s)
  (let ([s (symbol->string (syntax->datum s))])
    (apply string-append (for/list ([s (regexp-split "-" s)])
                           (string-append (string-upcase (substring s 0 1))
                                          (substring s 1))))))

(define-syntax (define-deprecated-wrapper stx)
  (syntax-case stx ()
    [(_ name)
     (identifier? #'name)
     (with-syntax ([op (camelCase #'name)])
       #'(define/contract (name js)
           (-> jsexpr? jsexpr?)
           (assert-20111205 'name)
           (dynamo-request op js)))]))

(define-deprecated-wrapper put-item)
(define-deprecated-wrapper get-item)
(define-deprecated-wrapper delete-item)
(define-deprecated-wrapper update-item)
(define-deprecated-wrapper batch-get-item)
(define-deprecated-wrapper batch-write-item)
(define-deprecated-wrapper query)
(define-deprecated-wrapper scan)
(define-deprecated-wrapper update-table)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ test
  (require rackunit
           racket/match
           "tests/data.rkt")
  (when (test-data-exists?)
    (test-case "dynamo"
      (define test (test/dynamo-table))
      (check-not-exn (λ () (create-table test 1 1
                                         "HashKey" "S"
                                         "RangeKey" "S")))
      (check-not-exn (λ () (list-tables #:limit 10)))
      (check-not-exn (λ () (describe-table test)))
      (let wait ()
        (match (describe-table test)
          [(hash-table ('Table (hash-table ('TableStatus status))))
           (printf "Table status is ~a" status)
           (cond [(equal? status "ACTIVE") (displayln ": ready!")]
                 [else (displayln " ... waiting for ACTIVE ... ")
                       (sleep 5)
                       (wait)])]))
      (check-not-exn
       (λ () (put-item (hasheq 'TableName test
                               'Item (hasheq 'HashKey (hasheq 'S "Hi")
                                             'RangeKey (hasheq 'S "world")
                                             'Foo (hasheq 'S "bar"))))))
      (sleep 2)
      (check-match (get-item (hasheq 'TableName test
                                     'Key (hasheq 'HashKeyElement (hasheq 'S "Hi")
                                                  'RangeKeyElement (hasheq 'S "world"))))
                   (hash-table ('Item (hash-table ('Foo (hash-table ('S "bar")))))))
      (check-not-exn (λ () (delete-table test))))))
