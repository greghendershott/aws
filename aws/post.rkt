;; Copyright (c) 2012-2022 by Greg Hendershott.
;; SPDX-License-Identifier: BSD-2-Clause

#lang at-exp racket/base

(require http/head
         http/request
         net/base64
         racket/contract/base
         racket/dict
         racket/format
         racket/match
         racket/math
         xml/xexpr
         "exn.rkt"
         "util.rkt")

(define/contract/provide (post-with-retry uri xs-post-data heads [try 1])
  ((string? dict? dict?)
   (exact-positive-integer?)
   . ->* .
   xexpr?)
  (define data (string->bytes/utf-8 (dict->form-urlencoded xs-post-data)))
  (log-aws-debug @~a{POST @data})
  (call/output-request
   "1.1" "POST" uri data #f heads
   (lambda (in h)
     (define e (read-entity/xexpr in h))
     (match (extract-http-code h)
       [200 e]
       [503 #:when (<= try 5)
            (define sleep-time (sqr try))
            (log-aws-warning
             @~a{AWS returned 503. Attempt @(add1 try) in @sleep-time seconds.})
            (sleep sleep-time)
            (post-with-retry uri xs-post-data heads (add1 try))]
       [_ (raise (header&response->exn:fail:aws
                  h e (current-continuation-marks)))]))))

(define/provide (set-next-token params token)
  (cons (list 'NextToken token)
        (filter (lambda (x) (not (equal? (car x) 'NextToken)))
                params)))

(define/provide (timestamp [seconds (current-seconds)])
  (seconds->gmt-8601-string 'T/Z seconds))

;; SDB docs:
;; "To ensure that you can read all the data you sent via REST, if a
;; response contains invalid XML characters, Amazon SimpleDB
;; automatically Base64-encodes the UTF-8 octets of the text."
;;
;; Example:
;; <Attribute>
;;  <Name encoding="base64">...</Name>
;;  <Value encoding="base64">...</Value>
;; </Attribute>
;;
;; "When designing your application, make sure to scrub any data for
;; invalid characters or design your application to handle
;; Base64-encoded results."
(define/contract/provide (attribute-xexpr->attrib-pair x)
  (xexpr? . -> . (list/c symbol? string?))
  ;; (Attribute () (Name () x) (Value () x))
  (match x
    [(list 'Attribute _ name val)
     (let ([name (maybe-decode name)]
           [val (maybe-decode val)])
       (list (if (string? name) (string->symbol name) name)
             val))]))

(define/provide (maybe-decode x)
  (match x
    [(list (or 'Name 'Value) attrs val)
     (match (assoc 'Encoding attrs)
       [(list 'Encoding "base64") (base64-decode val)]
       [else val])]))
