#lang racket

(require xml
         net/base64
         (planet gh/http/request)
         (planet gh/http/head)
         "exn.rkt"
         "util.rkt")

;;; Some functions used by both SQS and SDB
;;; This module probably needs a better name/organization.

(define/contract/provide (post-with-retry uri xs-post-data heads [try 1])
  ((string? (listof (list/c symbol? string?)) dict?)
   (exact-positive-integer?)
   . ->* .
   xexpr?)
  (define data (string->bytes/utf-8 (dict->form-urlencoded xs-post-data)))
  (log-debug (tr "POST" data))
  (call/output-request
   "1.1" "POST" uri data #f heads
   (lambda (in h)
     (define e (read-entity/xexpr in h))
     (match (extract-http-code h)
       [200 e]
       [503
        (if (<= try 5)
            (let ([sleep-time (sqr try)])   ;wait longer each time
              (log-info (format "SDB returned 503. Try ~a in ~a secs."
                                (add1 try) sleep-time))
              (sleep sleep-time)
              (post-with-retry uri xs-post-data heads (add1 try)))
            (error 'post-with-retry "too many 503 retries; giving up"))]
       [else
        (raise (header&response->exn:fail:aws
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
