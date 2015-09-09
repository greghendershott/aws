#lang at-exp racket/base

(require file/md5
         http/head
         http/request
         net/base64
         net/head
         net/uri-codec
         racket/contract/base
         racket/contract/region
         racket/dict
         racket/format
         racket/list
         racket/match
         racket/port
         racket/string
         (only-in sha bytes->hex-string)
         xml
         xml/path
         "exn.rkt"
         "keys.rkt"
         "pool.rkt"
         "sigv4.rkt"
         "util.rkt")

(provide (contract-out [s3-scheme (parameter/c string?)]
                       [s3-host (parameter/c string?)]
                       [s3-region (parameter/c string?)]
                       [s3-path-requests? (parameter/c boolean?)]
                       [path->mime-proc (parameter/c (-> path? string?))]))

(module+ test
  (require rackunit))

(define s3-scheme (make-parameter "http")) ;; (or/c "http" "https")

;; This probably should have been named `s3-endpoint` instead. See
;; "Endpoint" column in:
;; http://docs.aws.amazon.com/general/latest/gr/rande.html#s3_region
(define s3-host (make-parameter "s3.amazonaws.com"))

(define s3-region (make-parameter "us-east-1"))

;; Path style requests work only when the endpoint (what we call
;; `s3-host`) matches the location of the bucket. However they
;; permit less-restrictive names for US Standard buckets,
;; e.g. capital letters.
;;
;; Virtual host style requests work with the s3.amazonaws.com
;; endpoint regardless of location. Only downside is that the bucket
;; names are more restrictive -- see `valid-bucket-name?`.
;;
;; See http://docs.aws.amazon.com/AmazonS3/latest/dev/RESTAPI.html
(define s3-path-requests? (make-parameter #f))

;; See http://docs.amazonwebservices.com/AmazonS3/latest/dev/BucketRestrictions.html
(define/contract/provide (valid-bucket-name? s [dns-compliant? #t])
  (->* (string?) (boolean?) boolean?)
  (cond
   [dns-compliant?
    (and (<= 3 (string-length s) 63)
         (not (regexp-match #px"\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}" s))
         (for/and ([s (regexp-split #rx"\\." s)])
           (define (valid-first-or-last? c)
             (or (char-lower-case? c)
                 (char-numeric? c)))
           (define (valid-mid? c)
             (or (valid-first-or-last? c)
                 (equal? c #\-)))
           (define len (string-length s))
           (and (< 0 len)
                (valid-first-or-last? (string-ref s 0))
                (valid-first-or-last? (string-ref s (sub1 len)))
                (or (<= len 2)
                    (for/and ([c (substring s 1 (sub1 len))])
                      (valid-mid? c))))))]
   [else
    (and (<= 1 (string-length s) 255)
         (for/and ([c s])
           (or (char-numeric? c)
               (char-lower-case? c)
               (char-upper-case? c)
               (equal? c #\.)
               (equal? c #\-)
               (equal? c #\_))))]))

;; naming convention here:
;; "&" = elements as separate args/values
;; "+" = elements combined into a single arg/value

(define/contract/provide (bucket&path->uri b p)
  (-> string? string? string?)
  (cond [(s3-path-requests?)
         (string-append (s3-scheme) "://" (s3-host) "/" b "/" p)]
        [else
         (string-append (s3-scheme) "://" b "." (s3-host) "/" p)]))

(define/contract (bucket+path->uri b+p)
  (-> string? string?)
  (define-values (b p) (bucket+path->bucket&path b+p))
  (bucket&path->uri b p))

(define/contract/provide (bucket+path->bucket&path b+p)
  (-> string? (values string? string?))
  (match b+p
    [(regexp "^([^/]+?)/(.*?)$" (list _ b p)) (values b (or p ""))]
    [(regexp "^/") (error 'bucket+path->bucket&path
                          "Invalid bucket+path: ~e\n  Tip: Don't use a leading `/`"
                          b+p)]
    [_ (error 'bucket+path->bucket&path
              "Invalid bucket+path: ~e\n  Tip: Are you missing a trailing `/`?"
              b+p)]))

(define/contract/provide (bucket+path->bucket&path&uri b+p)
  (-> string? (values string? string? string?))
  (define-values (b p) (bucket+path->bucket&path b+p))
  (values b p (bucket&path->uri b p)))

(define (uri->host+port uri)
  (let-values ([(scheme host port path query fragment) (split-uri uri)])
    (string-append (or host "")
                   (if (and port (not (= port 80)))
                       (format ":~a" port)
                       ""))))

(define/contract (add-auth-heads* uri method heads body-hash)
  (-> string? string? dict? string? dict?)
  (ensure-have-keys)
  (let* ([heads (maybe-dict-set* heads
                                 'Host (uri->host+port uri)
                                 'Date (seconds->gmt-8601-string 'basic)
                                 'x-amz-content-sha256 body-hash)]
         [heads (dict-set heads
                          'Authorization
                          (aws-v4-authorization method
                                                uri
                                                heads
                                                body-hash
                                                (s3-region)
                                                "s3"))])
    heads))

(define/contract (add-auth-heads uri method [heads '()] [body #""])
  (->* (string? string?) (dict? bytes?) dict?)
  (add-auth-heads* uri method heads (sha256-hex-string body)))

(define/contract/provide (uri&headers b+p method [heads '()] [body #""])
  (->* (string? string?) (dict? bytes?) (values string? dict?))
  (define uri (bucket+path->uri b+p))
  (values uri (add-auth-heads uri method heads body)))



;;; create/list/delete buckets

;; `location` allows specifying a region. For the valid values, see the Region
;; column of http://docs.aws.amazon.com/general/latest/gr/rande.html#s3_region
;; Omitting this or supplying #f means US Standard, "us-east-1".
(define/contract/provide (create-bucket b [location #f])
  (->* (string?) ((or/c #f string?)) string?)
  (define entity
    (cond [location (string->bytes/utf-8
                     (xexpr->string
                      `(CreateBucketConfiguration
                        ([xmlns "http://s3.amazonaws.com/doc/2006-03-01/"])
                        (LocationConstraint () ,location))))]
          [else #""]))
  (put/bytes (string-append b "/") entity ""))

(define/contract/provide (delete-bucket b)
  (-> string? string?)
  (delete (string-append b "/")))

(define/contract/provide (list-buckets)
  (-> (listof string?))
  (define h (add-auth-heads "/" "GET"))
  (define xpr (call/input-request "1.1" "GET" (s3-host) h read-entity/xexpr))
  (se-path*/list '(Bucket Name) xpr))

(define/contract/provide (bucket-location bucket [default "us-east-1"])
  (->* (string?) (string?) string?)
  (parameterize ([s3-path-requests? #t])
    (match (get/proc (string-append bucket "/?location") read-entity/xexpr)
      [`(LocationConstraint ,_ ,location) location]
      [_ default])))

(define/contract/provide (get-acl bucket+path [heads '()])
  (->* (string?) (dict?) xexpr?)
  (define b+p (string-append bucket+path "?acl"))
  (get/proc b+p read-entity/xexpr heads))

(define/contract/provide (put-acl bucket+path acl)
  (-> string? xexpr? void)
  (define b+p (string-append bucket+path "?acl"))
  (define data (string->bytes/utf-8 (xexpr->string acl)))
  (void (put/bytes b+p data "application/xml")))

(define/contract/provide (ls/proc b+p f init [max-each 1000]
                                  #:delimiter [delimiter #f])
  (->* (string?
        (-> any/c (listof xexpr?) any/c)
        any/c)
       ((and/c integer? (between/c 1 1000))
        #:delimiter (or/c #f string?))
       any/c)
  (define-values (b p u) (bucket+path->bucket&path&uri b+p))
  (let loop ([marker ""]
             [cum init])
    ;; Ignore the path in the URI. This is an operation on the bucket.
    ;; Instead use the path for the prefix query parameter.
    (define qp (dict->form-urlencoded `((prefix ,p)
                                        (marker ,marker)
                                        (max-keys ,(~a max-each))
                                        ,@(if delimiter
                                              `((delimiter ,delimiter))
                                              '()))))
    (define uri (bucket+path->uri (string-append b "/?" qp)))
    (define h (add-auth-heads uri "GET"))
    (define xpr (call/input-request "1.1" "GET" uri  h
                                    (λ (in h)
                                      (check-response in h)
                                      (read-entity/xexpr in h))))
    (define contents (tags xpr 'Contents))
    (define prefixes (if delimiter
                         (tags xpr 'CommonPrefixes)
                         null))
    (define truncated? (equal? "true" (se-path* '(IsTruncated) xpr)))
    (define next-marker (and truncated?
                             (if delimiter
                                 (se-path* '(NextMarker) xpr)
                                 (se-path* '(Key) (last contents)))))
    (let* ([cum (if (null? contents)
                    cum
                    (f cum contents))]
           [cum (if (null? prefixes)
                    cum
                    (f cum prefixes))])
      (if truncated?
          (loop next-marker cum)
          cum))))

(define/contract/provide (ls b+p)
  (-> string? (listof string?))
  (map (λ (x) (first-tag-value x 'Key))
       (ls/proc b+p append '())))

(define/contract/provide (ll* b+p)
  (-> string? (listof (list/c xexpr? string? xexpr?)))
  (define-values (b p) (bucket+path->bucket&path b+p))
  (for/list ([x (in-list (ls/proc b+p append '()))])
    (define path (string-append b "/" (first-tag-value x 'Key)))
    (list x (head path) (get-acl path))))

(define/contract/provide (ll b+p)
  (-> string? (listof (list/c string? string? xexpr?)))
  (for/list ([x (in-list (ll* b+p))])
    (match-define (list contents heads acl) x)
    (list (first-tag-value contents 'Key) heads acl)))

(define/contract/provide (copy b+p/from b+p/to)
  (-> string? string? string?)
  (define-values (u h)
    (uri&headers b+p/to
                 "PUT"
                 (hasheq 'x-amz-copy-source
                         (string-append "/" (uri-encode b+p/from)))))
  (call/output-request "1.1" "PUT" u
                       (λ (out) (void))
                       0
                       h
                       (λ (in h)
                         (check-response in h)
                         ;; Just because check-response didn't raise
                         ;; an exception doesn't mean we're OK. Why?
                         ;; S3 docs say: "There are two opportunities
                         ;; for a copy request to return an error. One
                         ;; can occur when Amazon S3 receives the copy
                         ;; request and the other can occur while
                         ;; Amazon S3 is copying the files. If the
                         ;; error occurs before the copy operation
                         ;; starts, you receive a standard Amazon S3
                         ;; error. If the error occurs during the copy
                         ;; operation, the error response is embedded
                         ;; in the 200 response. This means that a 200
                         ;; response can contain either a success or
                         ;; an error. Make sure to design your
                         ;; application to parse the contents of the
                         ;; response and handle it appropriately."
                         (match (read-entity/bytes in h)
                           [(and (regexp "<Error>.*?</Error>") e)
                            (raise (header&response->exn:fail:aws
                                    h e (current-continuation-marks)))]
                           [_ (void)])
                         h)))



;;; delete/head objects in buckets

(define/contract/provide (delete bucket+path)
  (-> string? string?)
  (define-values (u h) (uri&headers bucket+path "DELETE" '()))
  (call/input-request "1.1" "DELETE" u h
                      (λ (in h)
                        (check-response in h)
                        h)))

(define/contract/provide (delete-multiple bucket paths)
  (-> string? (listof string?) string?)
  (when ((length paths) . > . 1000)
    (error 'delete-multiple "cannot delete more than 1000 items at a time"))
  (define data
    (string->bytes/utf-8 (xexpr->string
                          `(Delete
                            ,@(for/list ([p (in-list paths)])
                                `(Object () (Key () ,p)))))))
  (define-values (u h) (uri&headers (string-append bucket "/?delete")
                                    "POST"
                                    (hasheq 'Content-MD5 (bytes->Content-MD5 data))
                                    data))
  (call/output-request "1.1" "POST" u data (bytes-length data) h
                       (λ (in h)
                         (check-response in h)
                         ;; Just because 200 OK doesn't mean no error.
                         ;; Check for <Error> in the response.
                         (define e (read-entity/bytes in h))
                         (match e
                           [(regexp "<Error>.*?</Error>")
                            (raise (header&response->exn:fail:aws
                                    h e (current-continuation-marks)))]
                           [_ (void)])
                         h)))

(define/contract/provide (head bucket+path)
  (-> string? string?)
  (define-values (u h) (uri&headers bucket+path "HEAD"))
  (call/input-request "1.1" "HEAD" u h
                      (λ (in h)
                        (check-response in h)
                        h)))


;;; get

(define/contract/provide (get/proc bucket+path
                                   reader
                                   [heads '()]
                                   [beg #f]
                                   [end #f])
  (->* (string?
        (-> input-port? string? any/c))
       (dict?
        (or/c #f exact-nonnegative-integer?)
        (or/c #f exact-nonnegative-integer?))
       any/c)
  (define-values (u h) (uri&headers bucket+path
                                    "GET"
                                    (maybe-add-range-header heads beg end)))
  (call/input-request "1.1" "GET" u h
                      (λ (in h)
                        (check-response in h)
                        (reader in h))))

(define (maybe-add-range-header heads beg end)
  (cond [(and beg end (< beg end))
         (maybe-dict-set heads
                         'Range
                         (format "bytes=~a-~a"
                                 beg
                                 ;; Our `end' arg is EXclusive, but
                                 ;; Range header's end is INclusive.
                                 (sub1 end)))]
        [else heads]))

(define/contract/provide (get/bytes bucket+path
                                    [heads '()]
                                    [beg #f]
                                    [end #f])
  (->* (string?)
       (dict?
        (or/c #f exact-nonnegative-integer?)
        (or/c #f exact-nonnegative-integer?))
       bytes?)
  (get/proc bucket+path read-entity/bytes heads beg end))

(define/contract/provide (get/file bucket+path
                                   path
                                   [heads '()]
                                   #:mode [mode 'binary]
                                   #:exists [exists-flag 'error])
  (->* (string? path?)
       (dict?
        #:mode (or/c 'binary 'text)
        #:exists (or/c 'error 'append 'update 'replace 'truncate
                       'truncate/replace))
       void)
  (get/proc bucket+path
            (λ (in h)
              (call-with-output-file* path
                                      (λ (out)
                                        (read-entity/port in h out)
                                        (void))
                                      #:mode mode
                                      #:exists exists-flag))
            heads))



;;; put

(define (KB n) (* n 1024))
(define (MB n) (* n 1024 1024))

;; Docs say aws-chunk lengths should be at least 8 KB, recommend 64 KB.
(define aws-chunk-len-minimum (KB 8))
(define aws-chunk-len-default (KB 64))
(define aws-chunk-len/c
  (and/c exact-nonnegative-integer?
         (>=/c aws-chunk-len-minimum)))

(provide aws-chunk-len-minimum
         aws-chunk-len-default
         aws-chunk-len/c)

(define/contract/provide (put bucket+path
                              writer
                              data-len
                              mime-type
                              [heads '()]
                              #:chunk-len [chunk-len aws-chunk-len-default])
  (->* (string?
        (-> output-port? void?)
        exact-nonnegative-integer?
        string?)
       (dict?
        #:chunk-len aws-chunk-len/c)
       string?)
  (ensure-have-keys)
  (maybe-suggest-multipart-put data-len)
  (define u (bucket+path->uri bucket+path))
  (define h (add-auth-heads*
             u
             "PUT"
             (dict-set*
              heads
              'Expect                       "100-continue"
              ;; As best I can determine, contrary to the
              ;; documentation `Transfer-Encoding: chunked` is NOT
              ;; actually supported -- attempting this gives an
              ;; IncompleteBody error. I even tried wrapping the
              ;; aws-chunked Content-Encoding in another normal
              ;; chunked Transfer-Encoding -- but same error.
              ;; Fortunately we need to know the de-chunked length
              ;; up-front anyway for the x-amz-decoded-content-length
              ;; header, and it's not _too_ slow/painful to calculate
              ;; the Content-Length (including chunking overhead).
              'Content-Length               (chunked-content-length data-len chunk-len)
              'Content-Encoding             "aws-chunked"
              'Content-Type                 mime-type
              'x-amz-decoded-content-length data-len
              'x-amz-content-sha256         "STREAMING-AWS4-HMAC-SHA256-PAYLOAD")
             "STREAMING-AWS4-HMAC-SHA256-PAYLOAD"))
  (match-define (pregexp "Signature=([0-9a-f]{64})$" (list _ seed-signature))
    (dict-ref h 'Authorization))
  (define (chunk-writer out)
    ;; The user's `writer` proc writes directly to an output-port,
    ;; which was fine for sigv2. But now we need to intercept it for
    ;; sigv4 chunking. Solution: Use a Racket pipe, with a buffer
    ;; limited to `chunk-len` to conserve memory. Also, start the
    ;; writer thread only if/after a 100-continue.
    (define-values (pin pout) (make-pipe chunk-len 'chunk-pipe-in 'chunk-pipe-out))
    (thread (λ () (writer pout)))
    (parameterize ([current-output-port out])
      (define 8601-date (dict-ref h 'Date))
      (let display-chunks ([prev-sig seed-signature]
                           [data-len data-len])
        (define data (match (read-bytes (max 0 (min data-len chunk-len)) pin)
                       [(? eof-object?) #""]
                       [x x]))
        (define sts (string-join (list "AWS4-HMAC-SHA256-PAYLOAD"
                                       8601-date
                                       (credential-scope 8601-date
                                                         (s3-region)
                                                         "s3")
                                       prev-sig
                                       (sha256-hex-string #"")
                                       (sha256-hex-string data))
                                 "\n"))
        (define this-sig (signature sts 8601-date (s3-region) "s3"))
        (display-chunk data this-sig)
        (when (> data-len 0)
          (display-chunks this-sig (- data-len chunk-len))))))
  (call/output-request "1.1" "PUT" u chunk-writer #f h check-response))

(define/contract (display-chunk data sig)
  (-> bytes? string? any)
  (printf "~x;chunk-signature=~a\r\n" (bytes-length data) sig)
  (display data) (display "\r\n"))

(define (chunked-length len) ;amount display-chunk outputs
  (for/sum ([x (in-list (list (format "~x;chunk-signature=" len) 64 "\r\n"
                              len "\r\n"))])
    (match x
      [(? string? s) (string-length s)]
      [(? number? n) n])))

;; The Content-Length including overhead for aws-chunk metadata. Note:
;; This is about `Content-Encoding: aws-chunked` -- NOT
;; `Transfer-Encoding: chunked`, therefore the latter's optional
;; trailer-part and required final CRLF are N/A.
(define (chunked-content-length data-len chunk-len)
  (define-values (num-full-chunks penultimate-len)
    (quotient/remainder data-len chunk-len))
  (+ (* num-full-chunks (chunked-length chunk-len))
     (if (zero? penultimate-len) 0 (chunked-length penultimate-len))
     (chunked-length 0)))

(define/contract/provide (put/bytes bucket+path
                                    data
                                    mime-type
                                    [heads '()])
  (->* (string? bytes? string?) (dict?) string?)
  (define data-len (bytes-length data))
  (maybe-suggest-multipart-put data-len)
  (define-values (u h)
    (uri&headers bucket+path
                 "PUT"
                 (dict-set* heads
                            'Expect       "100-continue"
                            'Content-Type mime-type
                            'Content-MD5  (bytes->Content-MD5 data)
                            'Expect       "100-continue")
                 data))
  (call/output-request "1.1" "PUT" u data data-len h check-response))

(define/contract/provide (put/file bucket+path
                                   path
                                   #:mime-type [mime-type #f]
                                   #:mode [mode 'binary]
                                   #:chunk-len [chunk-len aws-chunk-len-default])
  (->* (string?
        path?)
       (#:mime-type (or/c #f string?)
        #:mode (or/c 'binary 'text)
        #:chunk-len aws-chunk-len/c)
       string?)
  (put bucket+path
       (λ (out) (call-with-input-file* path #:mode mode
                  (λ (in) (copy-port in out))))
       (file-size path)
       (or mime-type ((path->mime-proc) path))
       (hasheq 'Content-MD5 (file->Content-MD5 path)
               'Content-Disposition (path->Content-Disposition path))
       #:chunk-len chunk-len))

(define (maybe-suggest-multipart-put data-len)
  (when (> data-len (MB 100))
    (log-aws-warning (tr "S3 `put' where " data-len
                         "(when > 100 MB, consider using `multipart-put')"))))



;;; multipart put


;;; Multipart upload parts must be at least 5 MB
(define s3-multipart-size-minimum (MB 5))
(define s3-multipart-size-default (MB 5))
(define s3-multipart-size/c
  (and/c exact-positive-integer?
         (>=/c s3-multipart-size-minimum)))

;; Multipart upload part numbers start at 1, and there's a max of 10000
(define s3-multipart-number-minimum 1)
(define s3-multipart-number-maximum 10000)
(define s3-multipart-number/c (and/c exact-integer?
                                     (between/c s3-multipart-number-minimum
                                                s3-multipart-number-maximum)))

(provide s3-multipart-size-minimum
         s3-multipart-size-default
         s3-multipart-size/c
         s3-multipart-number-minimum
         s3-multipart-number-maximum
         s3-multipart-number/c)

(define/contract/provide (multipart-put b+p
                                        num-parts
                                        get-part
                                        [mime-type default-mime-type]
                                        [heads '()])
  (->* (string?
        s3-multipart-number/c
        (-> exact-nonnegative-integer? bytes?))
       (string?
        dict?)
       string?)
  (define upid (initiate-multipart-upload b+p mime-type heads))
  ;; ;; Simple version:
  ;; (define parts
  ;;   (for/list ([n (in-range num-parts)])
  ;;     (upload-part b+p upid (add1 n) (get-part n))))
  ;;
  ;; Using a worker pool:
  ;;
  ;; Some modest concurrency, such as 4, could help with upload
  ;; speeds. Too much could slow things down -- and use too much
  ;; memory: `get-part' is giving us bytes?, which for S3 multipart
  ;; upload must be at least 5 MB. So 4 workers is using 20 MB for
  ;; in-memory buffers. Easy, Trigger.
  (define parts
    (with-worker-pool (min 4 num-parts)
      (λ (pool)
        (for/list ([n (in-range num-parts)])
          (add-job pool
                   (λ ()
                     (upload-part b+p upid (add1 n) (get-part n)))))
        (get-results pool num-parts))))
  (complete-multipart-upload b+p upid parts)
  upid)

(define/contract/provide (multipart-put/file bucket+path
                                             path
                                             #:mime-type [mime-type #f]
                                             #:mode [mode 'binary]
                                             #:part-size [_part-size #f])
  (->* (string?
        path?)
       (#:mime-type (or/c #f string?)
        #:mode (or/c 'binary 'text)
        #:part-size s3-multipart-size/c)
       string?)
  (define-values (total-size part-size num-parts) (file-sizes-and-parts path _part-size))
  (multipart-put bucket+path
                 num-parts
                 (get-file-part path mode part-size)
                 (or mime-type ((path->mime-proc) path))
                 (hasheq 'Content-Disposition (path->Content-Disposition path))))

(define/contract (file-sizes-and-parts path _part-size)
  (-> path?
      (or/c #f s3-multipart-size/c)
      (values exact-nonnegative-integer?
              s3-multipart-size/c
              s3-multipart-number/c))
  (define total-size (file-size path))
  (define part-size (or _part-size (minimal-part-size total-size)))
  (define num-parts (ceiling (/ total-size part-size)))
  (values total-size part-size num-parts))


(define ((get-file-part path mode part-size) part-num)
  (call-with-input-file* path #:mode mode
    (λ (in)
      (file-position in (* part-num part-size))
      (read-bytes part-size in))))

(define/contract (minimal-part-size total-size)
  (-> exact-nonnegative-integer? s3-multipart-size/c)
  ;; Given a total size, return a part size that is at least
  ;; s3-multipart-size-minimum, and large enough that uploading
  ;; total-size won't require using more than
  ;; s3-multipart-number-maximum parts -- but otherwise as small as
  ;; possible.
  (cond [(< total-size (* s3-multipart-size-minimum s3-multipart-number-maximum))
         s3-multipart-size-minimum]
        [else
         (ceiling (/ total-size s3-multipart-number-maximum))]))

(module+ test
  (let ([boundary (* s3-multipart-size-minimum s3-multipart-number-maximum)])
    (check-equal?  (minimal-part-size 0)               s3-multipart-size-minimum)
    (check-equal?  (minimal-part-size 1)               s3-multipart-size-minimum)
    (check-equal?  (minimal-part-size (sub1 boundary)) s3-multipart-size-minimum)
    (check-equal?  (minimal-part-size boundary)        s3-multipart-size-minimum)
    (check-true (> (minimal-part-size (add1 boundary)) s3-multipart-size-minimum))))

(define/contract/provide (initiate-multipart-upload bucket+path mime-type heads)
  (-> string? string? dict? string?)
  (define b+p (string-append bucket+path "?uploads"))
  (define-values (u h) (uri&headers b+p
                                    "POST"
                                    (dict-set* heads 'Content-Type mime-type)))
  (define x (call/input-request "1.1" "POST" u h
                                (λ (in h)
                                  (check-response in h)
                                  (read-entity/xexpr in h))))
  (define upid (first-tag-value x 'UploadId))
  (log-aws-debug (tr "initiate-multipart-upload returned" upid))
  upid)

(define/contract/provide (upload-part bucket+path upid part bstr)
  (-> string? string? s3-multipart-number/c bytes?
      (cons/c s3-multipart-number/c string?))
  (log-aws-debug @~a{upload-part @part start})
  (define b+p (string-append bucket+path
                             "?partNumber=" (number->string part)
                             "&uploadId=" upid))
  (define-values (u h)
    (uri&headers b+p
                 "PUT"
                 (hasheq 'Expect "100-continue"
                         'Content-MD5 (bytes->Content-MD5 bstr))
                 bstr))
  (call/output-request "1.1" "PUT" u bstr (bytes-length bstr) h
                       (λ (in h)
                         (check-response in h)
                         (read-entity/bytes in h)
                         (define part-id (extract-field "ETag" h))
                         (log-aws-debug @~a{upload-part @part response @part-id})
                         (cons part part-id))))

(define/contract/provide (complete-multipart-upload bucket+path upid parts)
  (-> string? string? (listof (cons/c s3-multipart-number/c string?))
      xexpr?)
  (log-aws-debug (tr "complete-multipart-upload" upid parts))
  (define xm (parts->xml-bytes parts))
  (define b+p (string-append bucket+path "?uploadId=" upid))
  (define-values (u h) (uri&headers b+p "POST" '() xm))
  (call/output-request "1.1" "POST" u xm (bytes-length xm) h
                       (λ (in h)
                         (check-response in h)
                         ;; Even if we got a 200 OK status in the
                         ;; header, there may be a delay of minutes
                         ;; until S3 writes us the body response. It
                         ;; may send us space chars to help keep the
                         ;; connection open.
                         (define x (read-entity/xexpr in h))
                         ;; Check the response XML to see if
                         ;; truly succeeded.
                         (when (first-tag-value x 'Error)
                           (log-aws-fatal (tr x))
                           (raise (header&response->exn:fail:aws
                                   h x (current-continuation-marks))))
                         x)))

(define/contract (parts->xml-bytes parts)
  (-> (listof (cons/c s3-multipart-number/c string?)) bytes?)
  (string->bytes/utf-8
   (xexpr->string
    `(CompleteMultipartUpload
      ()
      ,@(for/list ([x (in-list (sort parts < #:key car))])
          (match x
            [(cons part (regexp "^\"(.+?)\"$" (list _ etag)))
             `(Part ()
                    (PartNumber () ,(number->string part))
                    (ETag () ,etag))]))))))

(define/contract/provide (list-multipart-uploads bucket)
  (-> string? xexpr?)
  ;; TODO: Handle multiple IsTruncated responses
  (define b+p (string-append bucket "/?uploads"))
  (define-values (u h) (uri&headers b+p "GET" '()))
  (call/input-request "1.1" "GET" u h
                      (λ (in h)
                        (check-response in h)
                        (read-entity/xexpr in h))))

(define/contract/provide (list-multipart-upload-parts bucket+path upid)
  (-> string? string? (listof xexpr?))
  (let loop ([parts '()]
             [marker ""])
    (define qp (dict->form-urlencoded `([uploadId ,upid]
                                        [max-parts "1000"]
                                        [part-number-marker ,marker])))
    (define b+p (string-append bucket+path "?" qp))
    (define-values (u h) (uri&headers b+p "GET" '()))
    (define xe (call/input-request "1.1" "GET" u h
                                   (λ (in h)
                                     (check-response in h)
                                     (read-entity/xexpr in h))))
    (match* ((first-tag-value xe 'IsTruncated) (append parts (tags xe 'Part)))
      [("true" parts) (loop parts (first-tag-value xe 'NextPartNumberMarker))]
      [(_      parts) parts])))

(define/contract/provide (abort-multipart-upload bucket+path upid)
  (-> string? string? void)
  (define b+p (string-append bucket+path"?uploadId=" upid))
  (define-values (u h) (uri&headers b+p "DELETE" '()))
  (call/input-request "1.1" "DELETE" u h
                      (λ (in h)
                        (check-response in h)
                        h)))

;; EXPERIMENTAL.
;; Return #f if no multipart upload initiated for b+p. Otherwise,
;; return a cons of an upload ID and a list of parts that were
;; successfully uploaded previously.
(define/contract/provide (incomplete-multipart-put/file b+p
                                                        path
                                                        #:mode [mode 'binary]
                                                        #:part-size [_part-size #f])
  (->* (string?
        path?)
       (#:mode (or/c 'binary 'text)
        #:part-size s3-multipart-size/c)
       (or/c #f
             (cons/c string?
                     (listof (cons/c s3-multipart-number/c string?)))))
  (define-values (total-size part-size num-parts) (file-sizes-and-parts path _part-size))
  (define get-part (get-file-part path mode part-size))
  (define (verify? part-num expected-etag)
    (define bstr (get-part (sub1 part-num)))
    (define file-etag (bytes->hex-string (md5 (open-input-bytes bstr) #f)))
    (equal? expected-etag file-etag))
  (define-values (bucket key) (bucket+path->bucket&path b+p))
  (for/or ([mpu (in-list (tags (list-multipart-uploads bucket) 'Upload))])
    (and
     (equal? key (se-path* '(Key) mpu))
     (let* ([upid (se-path* '(UploadId) mpu)]
            [parts (list-multipart-upload-parts b+p upid)])
       (define xs
         (filter-map
          values
          (for/list ([p (in-list parts)])
            (define this-part-num (string->number (se-path* '(PartNumber) p)))
            (define this-part-etag (second (se-path*/list '(ETag) p)))
            (and (<= this-part-num num-parts)
                 (verify? this-part-num this-part-etag)
                 (cons this-part-num this-part-etag)))))
       ;; If no parts matched, keep going in case it's another MUL for
       ;; the same b+p and path
       (and (not (empty? xs))
            (cons upid xs))))))

;; ;; FOR REPL DEV/TESTING ONLY
;; (define tmp (build-path "/tmp/multipart-put-test-file"))
;; (call-with-output-file tmp #:exists 'replace
;;   (λ (out)
;;     (for ([n (in-range 20)])
;;       (write-bytes (make-bytes s3-multipart-size-minimum n) out))))
;; (define (foo)
;;   (multipart-put/file "greghendershott/foo" tmp))

;; EXPERIMENTAL.
;; If incomplete-multipart-put/file returns a non #f result, use it to
;; resume the upload of the not-yet-uploaded parts.
(define/contract/provide (resume-multipart-put/file b+p
                                                    path
                                                    #:mime-type [mime-type #f]
                                                    #:mode [mode 'binary]
                                                    #:part-size [_part-size #f])
  (->* (string?
        path?)
       (#:mime-type (or/c #f string?)
        #:mode (or/c 'binary 'text)
        #:part-size s3-multipart-size/c)
       (or/c #f string?))
  (define-values (total-size part-size num-parts) (file-sizes-and-parts path _part-size))
  (log-aws-debug @~a{Checking for existing multipart upload of @path to @b+p})
  (match (incomplete-multipart-put/file b+p
                                        path
                                        #:mode mode
                                        #:part-size part-size)
    [#f #f]
    [(cons upid previously-uploaded-parts)
     (log-aws-debug @~a{Resuming existing upload @upid})
     (define get-part (get-file-part path mode part-size))
     (define parts
       (with-worker-pool (min 4 num-parts)
         (λ (pool)
           (for/list ([n (in-range num-parts)])
             (add-job
              pool
              (λ ()
                (match (assq (add1 n) previously-uploaded-parts)
                  [(cons part-num etag)
                   (log-aws-debug @~a{Part @part-num previously uploaded OK @etag})
                   (cons part-num (string-append "\"" etag "\""))]
                  [#f (upload-part b+p upid (add1 n) (get-part n))]))))
           (get-results pool num-parts))))
     (complete-multipart-upload b+p upid parts)
     upid]))



;;; misc utils

(define/contract (path->Content-Disposition path)
  (-> path-string? string?)
  (define-values (base name dir?) (split-path path))
  (format "attachment; filename=\"~a\"" name))

;; ETag and Content-MD5 utils

(define/contract (port->Content-MD5 in)
  (-> input-port? string?)
  (match (base64-encode (md5 in #f))
    ;; zap trailing \r\n
    [(regexp "^(.+?)\r\n$" (list _ s)) (bytes->string/utf-8 s)]))

(define/contract (bytes->Content-MD5 b)
  (-> bytes? string?)
  (port->Content-MD5 (open-input-bytes b)))

(define (file->Content-MD5 p)
  (-> path-string? string?)
  (call-with-input-file* p port->Content-MD5))



;;; query string request authentication

(define/contract/provide (sign-uri bucket+path method expires _heads)
  (-> string? string? expires/c dict? string?)
  (ensure-have-keys)
  (define-values (b p) (bucket+path->bucket&path bucket+path))
  (aws-v4-signed-uri method (bucket&path->uri b p) (s3-region) "s3" expires))



;;; mime

;;    my silent haiku
;;   imaginary rope tug
;;  chose my type crudely
(define ext->mime
  #hash(("txt" . "text/plain")
        ("htm" . "text/html")
        ("html" . "text/html")
        ("asp" . "text/html")
        ("apsx" . "text/html")
        ("jpg" . "image/jpeg")
        ("jpeg" . "image/jpeg")
        ("png" . "image/png")
        ("pdf" . "application/pdf")
        ("mp3" . "audio/mpeg3")
        ("wav" . "audio/x-wav")
        ("css" . "text/css")
        ("js" . "application/javascript")
        ("exe" . "application/octet-stream")
        ("msi" . "application/octet-stream")
        ("zip" . "application/x-compressed")
        ("gz" . "application/x-compressed")))

(define/contract (guess-mime-type path)
  (-> (or/c string? path?) string?)
  ;; Exception handler in case `split-path' barfs. Since need for
  ;; that, use to avoid else case for `match', too.
  (with-handlers ([exn:fail? (λ (exn) default-mime-type)])
    (define-values (base name dir?) (split-path path))
    (match (path-element->string name)
      [(regexp "\\.([^.]*)$" (list _ ext))
       (hash-ref ext->mime ext default-mime-type)])))

;; Let users replace.
(define path->mime-proc (make-parameter guess-mime-type))

(define default-mime-type "application/x-unknown-content-type")



;;; test

(module+ test
  (require net/url
           "tests/data.rkt")
  (test-case
   "valid-bucket-name?"
   (check-true (valid-bucket-name? "Aa1.-" #f))
   (check-false (valid-bucket-name? "A"))
   (check-true (valid-bucket-name? "1.1.1.1" #f))
   (check-false (valid-bucket-name? "1.1.1.1"))
   (check-false (valid-bucket-name? "255.255.255.255"))
   (check-true (valid-bucket-name? "111"))
   (check-true (valid-bucket-name? "a1a"))
   (check-true (valid-bucket-name? "1-1"))
   (check-false (valid-bucket-name? "-a-"))
   ;; Examples from AWS docs
   (check-true (valid-bucket-name? "myawsbucket"))
   (check-true (valid-bucket-name? "my.aws.bucket"))
   (check-true (valid-bucket-name? "myawsbucket.1"))
   (check-false (valid-bucket-name? ".myawsbucket"))
   (check-false (valid-bucket-name? "myawsbucket."))
   (check-false (valid-bucket-name? "my..examplebucket")))

  (test-case
   "bucket+path->bucket&path&uri"
   (parameterize ([s3-path-requests? #t])
     (define-values (b p u) (bucket+path->bucket&path&uri "bucket/path/name"))
     (check-equal? b "bucket")
     (check-equal? p "path/name")
     (check-equal? u "http://s3.amazonaws.com/bucket/path/name"))
   (parameterize ([s3-path-requests? #f])
     (define-values (b p u) (bucket+path->bucket&path&uri "bucket/path/name"))
     (check-equal? b "bucket")
     (check-equal? p "path/name")
     (check-equal? u "http://bucket.s3.amazonaws.com/path/name")))

  (test-case
   "guess-mime-type"
   (check-equal? (guess-mime-type "/path/to/file.txt") "text/plain")
   (check-equal? (guess-mime-type "/path/to/file.jpg") "image/jpeg")
   (check-equal? (guess-mime-type "/path/to/file.unknown") default-mime-type)
   (check-equal? (guess-mime-type "") default-mime-type))

  (test-case
   "path->Content-Disposition"
   (check-equal? (path->Content-Disposition "/foo/bar/test.txt")
                 "attachment; filename=\"test.txt\"")
   (when (equal? 'windows (system-path-convention-type))
     (check-equal? (path->Content-Disposition "c:\\foo\\bar\\test.txt")
                   "attachment; filename=\"test.txt\"")))

  ;; Append an incrementing suffix to bucket names used in
  ;; `test-bucket-ops`. Rationale: There can be intermittent errors --
  ;; such as a 404 NoSuchUpload for multipart uploads -- when the same
  ;; name is used to create, delete and recreate buckets within a
  ;; short amount of time. As best I understand it, this is due to
  ;; redirects and/or DNS updates, as described here in the AWS docs:
  ;; http://docs.aws.amazon.com/AmazonS3/latest/dev/UsingRouting.html
  ;; Because rapid reuse of a bucket name is not typical of real-world
  ;; usage -- just of unit tests -- I feel comfortable making the
  ;; tests _not_ reuse the same name, in order to avoid such
  ;; errors. Especially after spending many, many hours trying to
  ;; figure out the source of the intermittent errors, and looking for
  ;; any other reasonable way to address this.
  (define bucket-suffix 0)
  (define/contract (test-bucket-ops host region create-bucket-region)
    (-> string? string? (or/c #f string?) any)
    (time
     (parameterize ([s3-host host]
                    [s3-region region])
       (printf "~a: Host=~a Region=~a create-bucket-region=~a\n"
               (if (s3-path-requests?) "Path Style" "Virtual Hosted")
               (s3-host)
               (s3-region)
               create-bucket-region)

       (define bucket (format "~a-~a-~a"
                              (test/bucket) (s3-region) bucket-suffix))
       (set! bucket-suffix (add1 bucket-suffix))

       (ensure-have-keys)

       (define (member? x xs)
         (not (not (member x xs))))

       (create-bucket bucket create-bucket-region)
       (check-true (member? bucket (list-buckets)))

       (check-equal? (ls (string-append bucket "/")) '())
       (check-equal? (ls/proc (string-append bucket "/") (λ (v x) #t) #f) #f)

       (define b+p (string-append bucket "/" (test/path)))

       ;; put/bytes
       (define data #"Hello, world.")
       (put/bytes b+p data default-mime-type)
       (check-equal? (get/bytes b+p) data)
       (check-equal? (get/bytes b+p '() 0 4)
                     (subbytes data 0 4))
       (check-equal? (extract-http-code (head b+p)) 200)
       (check-true (xexpr? (get-acl b+p)))

       ;; ls and ls/proc:
       (check-equal? (list (test/path)) (ls (string-append bucket "/")))
       (check-equal? (ls/proc (string-append bucket "/") (λ (v x) (add1 v)) 0) 1)
       (let loop ([p (string-split (test/path) "/")] [prefix ""])
         (define more? (pair? (cdr p)))
         (ls/proc (string-append bucket "/" prefix)
                  #:delimiter "/"
                  (λ (v xs)
                    (check-equal? (length xs) 1)
                    (unless (null? xs)
                      (define x (car xs))
                      (check-equal? (car x) (if more? 'CommonPrefixes 'Contents))
                      (check-equal? (first-tag-value x (if more? 'Prefix 'Key))
                                    (string-append prefix (car p) (if more? "/" "")))))
                  (void))
         (when more?
           (loop (cdr p)
                 (string-append prefix (car p) "/"))))

       ;; delete and delete-multiple
       (define more-files
         (for/list ([i (in-range 1 4)])
           (define fn (string-append (test/path) "-" (number->string i)))
           (put/bytes (string-append bucket "/" fn)
                      (make-bytes i (char->integer #\x))
                      default-mime-type)
           fn))
       (check-equal? (ls (string-append bucket "/")) (cons (test/path) more-files))
       (delete (string-append bucket "/" (car more-files)))
       (check-equal? (ls (string-append bucket "/")) (cons (test/path) (cdr more-files)))
       (delete-multiple bucket (cdr more-files))
       (check-equal? (ls (string-append bucket "/")) (list (test/path)))

       ;; sign-uri
       (check-equal? (call/input-request "1.1" "GET"
                                         (sign-uri b+p "GET" 60 '())
                                         '()
                                         read-entity/bytes)
                     data)
       (let* ([expire 3]
              [uri (sign-uri b+p "GET" expire '())]
              [_ (sleep (add1 expire))]
              [x (call/input-request "1.1" "GET" uri '() read-entity/xexpr)])
         (check-equal? (first-tag-value x 'Message) "Request has expired"))

       ;; ACL
       (define acl (get-acl b+p))
       (put-acl b+p acl)
       (check-equal? (get-acl b+p) acl)

       ;; Copy
       (define b+p/copy (string-append b+p "-copy"))
       (copy b+p b+p/copy)
       (check-true (member? (string-append (test/path) "-copy")
                            (ls b+p/copy)))

       ;; Try put & get file, both normal and multipart
       (define (put&get-file put p)
         (define chksum (file->Content-MD5 p))
         (put)
         (get/file b+p p #:exists 'replace)
         (check-equal? (file->Content-MD5 p) chksum)
         (check-equal? (extract-http-code (head b+p)) 200)
         (check-true (member? (test/path) (ls b+p)))
         (check-true (member? (test/path)
                              (ls (string-append bucket "/"))))
         (check-true (member? (test/path)
                              (ls (string-append bucket
                                                 "/"
                                                 (substring (test/path) 0 3))))))
       (let ([p (build-path (find-system-path 'temp-dir) "s3-test-file.dat")]
             ;; For a faster test, make the test file size just large
             ;; enough to exercise mulitple chunks and parts.
             [size (+ (max aws-chunk-len-minimum s3-multipart-size-minimum)
                      (MB 1))])
         (with-output-to-file p #:exists 'replace
           (λ () (for ([i (in-range size)])
                   (display (bytes (random 255))))))
         (put&get-file (λ () (put/file b+p p
                                       #:mime-type "text/plain"
                                       #:chunk-len aws-chunk-len-minimum))
                       p)
         (put&get-file (λ () (multipart-put/file b+p p
                                                 #:mime-type "text/plain"
                                                 #:part-size s3-multipart-size-minimum))
                       p)
         (delete-file p))

       ;; Multipart upload: Do with enough parts to exercise the worker
       ;; pool of 4 threads. How about 8 parts.
       (define num-parts 8)
       (define part-size s3-multipart-size-minimum)
       (define (get-part-bytes n) (make-bytes part-size n))
       (multipart-put b+p num-parts get-part-bytes)
       (for ([i (in-range num-parts)])
         ;; This is also an opportunity to test Range requests ability:
         (check-equal? (get/bytes b+p '() (* i part-size) (* (add1 i) part-size))
                       (get-part-bytes i)))

       ;; Cleanup
       (delete b+p/copy)
       (delete b+p)
       (delete-bucket bucket)
       (void))))

  (when (test-data-exists?)
    (for ([timeout '(0 10)])
      (printf "=== Test using connection pool timeout of ~a sec ===\n" timeout)
      (parameterize ([current-pool-timeout timeout])
        (for ([path? '(#f #t)])
          (parameterize ([s3-path-requests? path?])
            (test-bucket-ops "s3.amazonaws.com"
                             "us-east-1"
                             #f)
            ;; eu-central-1 region only supports sigv4; good to test
            (test-bucket-ops "s3-eu-central-1.amazonaws.com"
                             "eu-central-1"
                             "eu-central-1")))))

    (test-case
     "100-continue"
     ;; Confirm that the http collection's handling of 100-continue is
     ;; working as expected with S3.
     ;;
     ;; Make a PUT request with a Content-Length vastly bigger than S3
     ;; will accept. Check that S3 responds as expected with "400 Bad
     ;; Request". Then check that our `writer' proc was NOT
     ;; called. (Don't worry, if it is called, we don't actually write
     ;; anything at all; definitely not the huge number of bytes.)
     (ensure-have-keys)
     (create-bucket (test/bucket))
     (define writer-called? #f)
     (check-exn
      exn:fail:aws?
      (λ ()
        (put (string-append (test/bucket) "/" (test/path))
             (λ (_) (set! writer-called? #t))
             (expt 2 64) ;16,384 petabytes _might_ be too large?
             "text/plain")))
     (check-false writer-called?)
     (delete-bucket (test/bucket))
     (void))))
