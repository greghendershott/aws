#lang racket

(require (planet gh/http)
         (planet gh/aws/keys)
         file/sha1
         "hmac-sha256.rkt")
         
(define/contract (canonical-request method
                                    uri
                                    heads
                                    body)
  (string? string? dict? bytes? . -> . string?)
  (string-join
   (list method
         (uri->path uri)
         (uri->query uri)
         (string-join (sort (for/list ([(k v) (in-dict heads)])
                                (format "~a:~a\n"
                                        (string-downcase (symbol->string k))
                                        v))
                            string<=?)
                      "")
         (string-join (sort (for/list ([k (in-dict-keys heads)])
                                (string-downcase (symbol->string k)))
                            string<=?)
                      ";")
         (bytes->hex-string (SHA256 body)))
   "\n"))

(define (uri->path u)
  (define-values (scheme host port path query fragment) (split-uri u))
  (or path "/"))

(define (uri->query u)
  (define-values (scheme host port path query fragment) (split-uri u))
  (or query ""))

(define (string-to-sign 8601-date region service canonical-request)
  (string-join
   (list "AWS4-HMAC-SHA256"
         8601-date
         (credential-scope 8601-date region service)
         (bytes->hex-string (SHA256 (string->bytes/utf-8 canonical-request))))
   "\n"))

(define/contract (credential-scope 8601-date region service)
  (string? string? string? . -> . string?)
  (string-join
   (list (8601-date-only 8601-date)
         region
         service
         "aws4_request")
   "/"))

(define/contract (8601-date-only s)
  (string? . -> . string?)
  (match s [(regexp "^(.+?)T" (list _ d)) d]))

;; Value for Authorization header
(define/contract (authorization string-to-sign heads 8601-date region service)
  (string? dict? string? string? string? . -> . string?)
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
  (string? string? string? string? . -> . string?)
  (bytes->hex-string
   (HMAC-SHA256 (derived-signing-key 8601-date region service)
                (string->bytes/utf-8 string-to-sign))))

(define/contract (derived-signing-key 8601-date region service)
  (string? string? string? . -> . bytes?)
  (define k-date (HMAC-SHA256 (bytes-append #"AWS4"
                                            (string->bytes/utf-8 (private-key)))
                              (string->bytes/utf-8 (8601-date-only 8601-date))))
  (define k-region (HMAC-SHA256 k-date (string->bytes/utf-8 region)))
  (define k-service (HMAC-SHA256 k-region (string->bytes/utf-8 service)))
  (define k-signing (HMAC-SHA256 k-service #"aws4_request"))
  k-signing)

(define/contract (aws-v4-authorization method uri heads body
                                       region service)
  (string? string? dict? bytes? string? string?
   . -> . string?)
  (ensure-have-keys)
  (define date
    (match (dict-ref heads 'Date)
      [(pregexp "^(\\d{8}T\\d{6}Z)" (list _ d)) d]
      [else (error 'aws-v4-authorization
                   "Must be 8601 basic date: YYYYMMDDTHHMMSSZ")]))
  (authorization (string-to-sign date region service
                                 (canonical-request method uri heads body))
                 heads date region service))

;; (aws-v4-authorization 'get
;;                       "/"
;;                       (hash 'Date "20120821T230000Z"
;;                             'Host "host")
;;                       ""
;;                       "us-east-1"
;;                       "s3")
                              
(provide aws-v4-authorization)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

(require rackunit
         (planet gh/http/head))

(parameterize ([private-key "wJalrXUtnFEMI/K7MDENG+bPxRfiCYEXAMPLEKEY"]
               [public-key "AKIDEXAMPLE"])
  (define (aws-test-file name)
    (regexp-replace* 
     "\r\n"                             ;DOS files with CRLF
     (file->string (build-path 'same "vendor" "aws4_testsuite" name))
     "\n"))

  (define heads (hash 'Date "Mon, 09 Sep 2011 23:36:00 GMT"
                      'Host "host.foo.com"
                      'Zoo "foobar,zoobar,zoobar"))
  (define 8601-date "20110909T233600Z")
  (define region "us-east-1")
  (define service "host")
  (define creq (canonical-request "POST" "/" heads #""))
  (test-case
   "canonical-request"
   (check-equal? creq
                 (aws-test-file "get-header-key-duplicate.creq")))

  (define sts (string-to-sign 8601-date region service creq))
  (test-case
   "string-to-sign"
   (check-equal? sts
                 (aws-test-file "get-header-key-duplicate.sts")))

  (define authz (authorization sts heads 8601-date region service))
  (test-case
   "signature"
   (check-equal? authz
                 (aws-test-file "get-header-key-duplicate.authz")))

  ;; Amazon provides a number of test files to check the series of steps.
  ;; Files have same base name, with extensions .req .creq .sts .authz.
  ;; Let's check ourselves against them.
  (define (req->sreq base)
    ;; Most annoying part of this is parsing their original .req:
    (define xs
      (file->lines (build-path 'same "vendor" "aws4_testsuite"
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

    (define creq (canonical-request method path heads body))
    (check-equal? creq (aws-test-file (string-append base ".creq")))

    (define sts (string-to-sign date region service creq))
    (check-equal? sts (aws-test-file (string-append base ".sts")))

    (define authz (authorization sts heads date region service))
    (check-equal? authz (aws-test-file (string-append base ".authz")))

    (void) ;;(values method path heads body creq date)
    )

  (req->sreq "get-utf8")
  (req->sreq "post-vanilla")
  (req->sreq "post-vanilla-query")
  (req->sreq "get-vanilla")
  (req->sreq "get-vanilla-query")
  (req->sreq "post-x-www-form-urlencoded")
  (req->sreq "post-x-www-form-urlencoded-parameters")
  (req->sreq "get-space")
  ;; (req->sreq "get-header-key-duplicate")
  ;; (req->sreq "get-header-value-order")
  ;; (req->sreq "get-relative-relative")
  ;; (req->sreq "get-relative")
  ;; (req->sreq "get-slash-dot-slash")
  ;; (req->sreq "get-slash-pointless-dot")
  ;; (req->sreq "get-slash")
  ;; (req->sreq "get-slashes")
  ;; (req->sreq "get-unreserved")
  ;; (req->sreq "get-vanilla-empty-query-key")
  ;; (req->sreq "get-vanilla-query-order-key")
  ;; (req->sreq "get-vanilla-query-order-key-case")
  ;; (req->sreq "post-vanilla-query-space")
  ;; (req->sreq "get-vanilla-ut8-query")

  )
