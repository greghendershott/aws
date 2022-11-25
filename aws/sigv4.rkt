;; Copyright (c) 2012-2022 by Greg Hendershott.
;; SPDX-License-Identifier: BSD-2-Clause

#lang racket/base

(require http
         net/uri-codec
         racket/contract/base
         racket/contract/region
         racket/dict
         racket/list
         racket/match
         racket/set
         racket/string
         sha
         "keys.rkt"
         "util.rkt")

(provide aws-v4-authorization
         add-v4-auth-heads
         aws-v4-signed-uri
         expires/c
         sha256-hex-string
         credential-scope
         signature)

(module+ test
  (require rackunit))

(define sha256-hex-string (compose1 bytes->hex-string sha256))

(define/contract (canonical-request method
                                    uri
                                    heads
                                    sha256-hex-str)
  (-> string? string? dict? string? string?)
  (string-join
   (list method
         (canonical-path uri)
         (canonical-query uri)
         (canonical-heads heads)
         (canonical-signed-heads heads)
         sha256-hex-str)
   "\n"))

(define (canonical-path u)
  (define-values (scheme host port path query fragment) (split-uri u))
  (or path "/"))

(define (canonical-query u)
  (define-values (scheme host port path query fragment) (split-uri u))
  (string-join
   (sort (for/list ([(k v) (in-dict (form-urlencoded->alist (or query "")))])
           (format "~a=~a"
                   (our-encode (symbol->string k))
                   (if v (our-encode v) "")))
         string<=?)
   "&"))

(define (canonical-heads heads)
  (string-join (sort (for/list ([(k v) (in-dict heads)])
                       (format "~a:~a\n"
                               (string-downcase (symbol->string k))
                               v))
                     string<=?)
               ""))

(define (canonical-signed-heads heads)
  (string-join (sort (for/list ([k (in-dict-keys heads)])
                       (string-downcase (symbol->string k)))
                     string<=?)
               ";"))

(define (number->hex-string number)
  (define (hex n) (string-ref "0123456789ABCDEF" n))
  (string #\% (hex (quotient number 16)) (hex (modulo number 16))))

(define self-chars
  (list->seteq
   (append*
    (for/list ([x (in-list `(#\% #\_ #\- #\. #\~ [#\A . #\Z] [#\a . #\z] [#\0 . #\9]))])
      (match x
        [(cons from thru)
         (for/list ([i (in-range (char->integer from) (add1 (char->integer thru)))]) i)]
        [(? char? char) (list (char->integer char))])))))

(define (our-encode in #:encode-slash? [encode-slash? #t])
  (define int->string (compose1 string integer->char))
  (string-append*
   (for/list ([byte (in-bytes (string->bytes/utf-8 in))])
     (cond [(set-member? self-chars byte)  (int->string byte)]
           [(eq? byte (char->integer #\/)) (if encode-slash? "%2F" (int->string byte))]
           [else                           (number->hex-string byte)]))))

(module+ test
  (check-equal? (our-encode "AaZz09") "AaZz09")
  (check-equal? (our-encode "/path/to" #:encode-slash? #t) "%2Fpath%2Fto")
  (check-equal? (our-encode "/path/to" #:encode-slash? #f) "/path/to")
  (check-equal? (our-encode "ሴ") "%E1%88%B4"))

(define (string-to-sign 8601-date region service canonical-request)
  (string-join
   (list "AWS4-HMAC-SHA256"
         8601-date
         (credential-scope 8601-date region service)
         (sha256-hex-string (string->bytes/utf-8 canonical-request)))
   "\n"))

(define/contract (credential-scope 8601-date region service)
  (-> string? string? string? string?)
  (string-join
   (list (8601-date-only 8601-date)
         region
         service
         "aws4_request")
   "/"))

(define/contract (8601-date-only s)
  (-> string? string?)
  (match s
    [(pregexp "^(\\d{8})" (list _ d)) d]
    [_ (error '8601-date-only "expected an 8601 date or datetime")]))

;; Value for Authorization header
(define/contract (authorization string-to-sign heads 8601-date region service)
  (-> string? dict? string? string? string? string?)
  (ensure-have-keys)
  (string-append
   "AWS4-HMAC-SHA256 "
   "Credential=" (public-key) "/" (8601-date-only 8601-date) "/" region "/"
   service "/" "aws4_request, "
   "SignedHeaders=" (string-join (sort (for/list ([k (in-dict-keys heads)])
                                           (string-downcase (symbol->string k)))
                                       string<=?)
                                 ";")
   ", "
   "Signature=" (signature string-to-sign 8601-date region service)))

(define/contract (signature string-to-sign 8601-date region service)
  (-> string? string? string? string? string?)
  (bytes->hex-string
   (hmac-sha256 (derived-signing-key 8601-date region service)
                (string->bytes/utf-8 string-to-sign))))

(define/contract (derived-signing-key 8601-date region service)
  (-> string? string? string? bytes?)
  (ensure-have-keys)
  (define k-date (hmac-sha256 (bytes-append #"AWS4"
                                            (string->bytes/utf-8 (private-key)))
                              (string->bytes/utf-8 (8601-date-only 8601-date))))
  (define k-region (hmac-sha256 k-date (string->bytes/utf-8 region)))
  (define k-service (hmac-sha256 k-region (string->bytes/utf-8 service)))
  (define k-signing (hmac-sha256 k-service #"aws4_request"))
  k-signing)

(define/contract (aws-v4-authorization method uri heads sha256-hex-str region service)
  (-> string? string? dict? string? string? string? string?)
  (define date
    (match (dict-ref heads 'Date)
      [(pregexp "^(\\d{8}T\\d{6}Z)" (list _ d)) d]
      [_ (error 'aws-v4-authorization
                "Date header must be 8601 basic date: YYYYMMDDTHHMMSSZ")]))
  (authorization (string-to-sign date region service
                                 (canonical-request method uri heads sha256-hex-str))
                 heads date region service))

#;
(aws-v4-authorization "get"
                      "/"
                      (hash 'Date "20120821T230000Z"
                            'Host "host")
                      ""
                      "us-east-1"
                      "s3")

;; A wrapper for aws-v4-authorization that is "update headers"
;; oriented. Not only does it add the full Authorization header. It
;; handles the case of obtaining credentials from EC2 instance
;; meta-data -- ensuring they are refreshed and adding the required
;; token header.
(define/contract (add-v4-auth-heads #:heads   heads
                                    #:method  method
                                    #:uri     uri
                                    #:sha256  sha256
                                    #:region  region
                                    #:service service)
  (-> #:heads   dict?
      #:method  string?
      #:uri     string?
      #:sha256  string?
      #:region  string?
      #:service string?
      dict?)
  (let ([heads (ensure-ec2-instance-credentials-and-add-token-header heads)])
    (dict-set heads
              'Authorization
              (aws-v4-authorization method
                                    uri
                                    heads
                                    sha256
                                    region
                                    service))))


(module+ test
  (require rackunit
           racket/file
           racket/runtime-path
           http/head
           "tests/data.rkt")
  (define-runtime-path test-suite-dir "vendor/aws4_testsuite")
  (parameterize ([private-key "wJalrXUtnFEMI/K7MDENG+bPxRfiCYEXAMPLEKEY"]
                 [public-key "AKIDEXAMPLE"])
    (define (aws-test-file name)
      (regexp-replace*
       "\r\n"                           ;DOS files with CRLF
       (file->string (build-path test-suite-dir name))
       "\n"))

    (define heads (hash 'Date "Mon, 09 Sep 2011 23:36:00 GMT"
                        'Host "host.foo.com"
                        'Zoo "foobar,zoobar,zoobar"))
    (define 8601-date "20110909T233600Z")
    (define region "us-east-1")
    (define service "host")
    (define creq (canonical-request "POST" "/" heads (sha256-hex-string #"")))
    (check-equal? creq
                  (aws-test-file "get-header-key-duplicate.creq"))

    (define sts (string-to-sign 8601-date region service creq))
    (check-equal? sts
                  (aws-test-file "get-header-key-duplicate.sts"))

    (define authz (authorization sts heads 8601-date region service))
    (check-equal? authz
                  (aws-test-file "get-header-key-duplicate.authz"))

    ;; Amazon provides a number of test files to check the series of steps.
    ;; Files have same base name, with extensions .req .creq .sts .authz.
    ;; Let's check ourselves against them.
    (define-syntax-rule (req->sreq base)
      (begin
        ;; Most annoying part of this is parsing their original .req:
        (define xs
          (file->lines (build-path test-suite-dir
                                   (string-append base ".req"))))
        (define-values (method path)
          (match (car xs)
            [(pregexp "^(.+?) (.+?) (?i:http/\\d\\.\\d)$" (list _ m p))
             (values m p)]))
        (define-values (head/string body)
          (let loop ([s ""]
                     [xs (cdr xs)])
            (cond
              [(empty? xs) (values s #"")]
              [(string=? (car xs) "") (values (string-append s "\r\n")
                                              (string->bytes/utf-8
                                               (string-join (cdr xs) "\r\n")))]
              [else (loop (string-append s (car xs) "\r\n")
                          (cdr xs))])))
        ;; And now we're ready to get going:
        (define heads (heads-string->dict head/string ","))
        (define date (seconds->gmt-8601-string
                      'basic
                      (gmt-string->seconds (or (dict-ref heads 'Date #f)
                                               (dict-ref heads 'date #f)
                                               (dict-ref heads 'DATE #f)))))

        (define creq (canonical-request method path heads (sha256-hex-string body)))
        (check-equal? creq (aws-test-file (string-append base ".creq")))

        (define sts (string-to-sign date region service creq))
        (check-equal? sts (aws-test-file (string-append base ".sts")))

        (define authz (authorization sts heads date region service))
        (check-equal? authz (aws-test-file (string-append base ".authz")))))

    (req->sreq "get-utf8")
    (req->sreq "post-vanilla")
    (req->sreq "post-vanilla-query")
    (req->sreq "get-vanilla")
    (req->sreq "get-vanilla-query")
    (req->sreq "post-x-www-form-urlencoded")
    (req->sreq "post-x-www-form-urlencoded-parameters")
    (req->sreq "get-space")
    (req->sreq "get-unreserved")
    (req->sreq "get-vanilla-empty-query-key")
    (req->sreq "get-vanilla-query-order-key")
    (req->sreq "get-vanilla-query-order-key-case")
    (req->sreq "get-vanilla-ut8-query")

    ;; I'm not convinced that this signature module is the appropriate
    ;; place to be cleansing URI paths:
    ;;   (req->sreq "get-slash")
    ;;   (req->sreq "get-slashes")
    ;;   (req->sreq "get-relative-relative")
    ;;   (req->sreq "get-relative")
    ;;   (req->sreq "get-slash-dot-slash")
    ;;   (req->sreq "get-slash-pointless-dot")

    ;; I don't understand the expected behavior for this test:
    ;;   (req->sreq "post-vanilla-query-space")

    ;; The following test won't pass because heads-string->dict is
    ;; case-sensitive with respect to keys:
    ;;   (req->sreq "get-header-key-duplicate")

    ;; The following test won't pass because heads-string->dict doesn't
    ;; provide an easy way to split duplicate values, and I'm lazy:
    ;;   (req->sreq "get-header-value-order")
    ))

(define expires/c (and/c exact-positive-integer?
                         (between/c 1 604800)))

(define/contract (aws-v4-signed-uri method uri region service expires [8601-date #f])
  (->* (string? string? string? string? expires/c) (string?) string?)
  (define-values (scheme host port path query fragment) (split-uri uri))
  (define date (or 8601-date (seconds->gmt-8601-string 'basic)))
  (define qps (hasheq 'X-Amz-Algorithm "AWS4-HMAC-SHA256"
                      'X-Amz-Credential (string-join (list (public-key)
                                                           (8601-date-only date)
                                                           region
                                                           service
                                                           "aws4_request")
                                                     "/")
                      'X-Amz-Date date
                      'X-Amz-Expires expires
                      'X-Amz-SignedHeaders "host"))
  (define creq (canonical-request method
                                  (combine-uri scheme
                                               host
                                               port
                                               path
                                               (dict->form-urlencoded qps)
                                               fragment)
                                  `([Host . ,host])
                                  "UNSIGNED-PAYLOAD"))
  (define sts (string-to-sign date region service creq))
  (define sig (signature sts date region service))
  (combine-uri scheme
               host
               port
               path
               (dict->form-urlencoded (dict-set qps 'X-Amz-Signature sig))
               fragment))
