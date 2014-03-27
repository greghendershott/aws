#lang racket

(require net/head
         net/base64
         net/uri-codec
         file/md5
         xml
         http/request
         http/head
         "util.rkt"
         "exn.rkt"
         "keys.rkt"
         "pool.rkt"
         )

(provide (contract-out [s3-scheme parameter/c]
                       [s3-host parameter/c]
                       [s3-path-requests? parameter/c]))

(define s3-scheme (make-parameter "http")) ;; (or/c "http" "https")

;; This probably should have been named `s3-endpoint` instead.
;; See "Endpoint" column in http://docs.aws.amazon.com/general/latest/gr/rande.html#s3_region
(define s3-host (make-parameter "s3.amazonaws.com"))

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
  ((string?) (boolean?) . ->* . boolean?)
  (cond
   [dns-compliant?
    (and (<= 3 (string-length s)) (<= (string-length s) 63)
         (not (regexp-match #px"\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}" s))
         (for/and ([s (regexp-split #rx"\\." s)])
           (define (valid-first-or-last? c)
             (or (char-lower-case? (string-ref s 0))
                 (char-numeric? (string-ref s 0))))
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
    (and (<= (string-length s) 255)
         (for/and ([c s])
           (or (char-numeric? c)
               (char-lower-case? c)
               (char-upper-case? c)
               (equal? c #\.)
               (equal? c #\-)
               (equal? c #\_))))]))

;; Use the URI format where the bucket is part of the path, not part
;; of the host name, because that supports bucket names that contain
;; capital letters.
(define/contract/provide (bucket&path->uri b p)
  (string? string? . -> . string?)
  (cond [(s3-path-requests?)
         (string-append (s3-scheme) "://" (s3-host) "/" b "/" p)]
        [else
         (string-append (s3-scheme) "://" b "." (s3-host) "/" p)]))

(define/contract/provide (bucket+path->bucket&path b+p)
  (string? . -> . (values string? string?))
  (match b+p
    [(regexp "^([^/]+?)/(.*?)$" (list _ b p)) (values b (or p ""))]
    [(regexp "^/") (error 'bucket+path->bucket&path "don't use leading /")]
    [else (error 'bucket+path->bucket&path "invalid bucket+path: ~e" b+p)]))

(define/contract/provide (bucket+path->bucket&path&uri b+p)
  (string? . -> . (values string? string? string?))
  (define-values (b p) (bucket+path->bucket&path b+p))
  (values b p (bucket&path->uri b p)))

(define/contract (make-authorization-header canonical)
  (string? . -> . dict?)
  (ensure-have-keys)
  (hash 'Authorization
        (string-append "AWS " (public-key) ":" (sha1-encode canonical))))

(define symbol-downcase
  (compose1 string->symbol string-downcase symbol->string))

(define/contract (canonical-amz-headers-string h)
  (dict? . -> . string?)
  ;; Steps below are quoted or paraphrased from S3 docs:
  ;; 1. Lower-case header name.
  ;; 2. Headers sorted by header name.
  ;; 3. The values of headers whose names occur more than once should
  ;; be white space-trimmed and concatenated with comma separators to
  ;; be compliant with section 4.2 of RFC 2616.
  ;; 4. Remove any whitespace around the colon in the header
  ;; 5. Remove any newlines ('\n') in continuation lines
  ;; 6. Separate headers by newlines ('\n')
  (let* ([xs (for/list ([(k v) (in-dict h)]
                        #:when (regexp-match? #rx"^(?i:x-amz-)"
                                              (symbol->string k)))
               (format "~a:~a\n"                            ;4,6
                       (string-downcase (symbol->string k)) ;1
                       (regexp-replace #rx"\n" v ",")))]    ;3,5
         [xs (sort xs string<?)])                           ;2
    (string-join xs "")))

(define/contract (canonical-string-to-sign bucket+path method date heads)
  (string? string? string? dict? . -> . string?)
  (string-append method "\n"
                 (dict-ref heads 'Content-MD5 "") "\n"
                 (dict-ref heads 'Content-Type "") "\n"
                 date "\n"
                 (canonical-amz-headers-string heads) ;provides its own \n
                 "/" bucket+path))

(define/contract (make-date+authorization-headers bucket+path method heads)
  (string? string? dict? . -> . dict?)
  (ensure-have-keys)
  (define date (seconds->gmt-string))
  (dict-set (make-authorization-header (canonical-string-to-sign bucket+path
                                                                 method
                                                                 date
                                                                 heads))
            'Date
            date))

(define/contract/provide (uri&headers b+p method heads)
  (string? string? dict? . -> . (values string? dict?))
  (define-values (b p u) (bucket+path->bucket&path&uri b+p))
  (define extra-heads (make-date+authorization-headers b+p method heads))
  (define h (dict-merge heads extra-heads))
  (values u h))

(define (dict-merge d1 d2)
  (apply dict-set* (cons d1 (flatten (dict->list d2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; `location` allows specifying a region. For the valid values, see the Region
;; column of http://docs.aws.amazon.com/general/latest/gr/rande.html#s3_region
;; Omitting this or supply #f means US Standard, "us-east-1".
(define/contract/provide (create-bucket b [location #f])
  ((string?) ((or/c #f string?)) . ->* . string?)
  (define entity
    (cond [location (string->bytes/utf-8
                     (xexpr->string
                      `(CreateBucketConfiguration
                        ([xmlns "http://s3.amazonaws.com/doc/2006-03-01/"])
                        (LocationConstraint () ,location))))]
          [else #""]))
  (put/bytes (string-append b "/") entity ""))

(define/contract/provide (delete-bucket b)
  (string? . -> . string?)
  (delete (string-append b "/")))

(define/contract/provide (list-buckets)
  (-> (listof string?))
  (define h (make-date+authorization-headers "" "GET" '()))
  (define xpr (call/input-request "1.1" "GET" (s3-host) h read-entity/xexpr))
  (map third (tags xpr 'Name 'Bucket)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define/contract/provide (delete bucket+path)
  (string? . -> . string?)
  (define-values (u h) (uri&headers bucket+path "DELETE" '()))
  (call/input-request "1.1" "DELETE" u h
                      (lambda (in h)
                        (check-response in h)
                        h)))

(define/contract/provide (delete-multiple bucket paths)
  (string? (listof string?) . -> . string?)
  (when ((length paths) . > . 1000)
    (error 'delete-multiple "cannot delete more than 1000 items at a time"))
  (define data
    (string->bytes/utf-8 (xexpr->string
                          `(Delete
                            ,@(for/list ([p (in-list paths)])
                                `(Object () (Key () ,p)))))))
  (define-values (u h) (uri&headers (string-append bucket "/?delete") "POST"
                                    (dict-set* '()
                                               'Content-MD5 (bytes->Content-MD5 data))))
  (call/output-request "1.1" "POST" u data (bytes-length data) h
                       (lambda (in h)
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
  (string? . -> . string?)
  (define-values (u h) (uri&headers bucket+path "HEAD" '()))
  (call/input-request "1.1" "HEAD" u h
                      (lambda (in h)
                        (check-response in h)
                        h)))

(define/contract/provide (get-acl bucket+path [heads '()])
  ((string?) (dict?) . ->* . xexpr?)
  (define b+p (string-append bucket+path "?acl"))
  (get/proc b+p read-entity/xexpr heads))

(define/contract/provide (put-acl bucket+path acl)
  (string? xexpr? . -> . void)
  (define b+p (string-append bucket+path "?acl"))
  (define data (string->bytes/utf-8 (xexpr->string acl)))
  (void (put/bytes b+p data "application/xml")))

(define/contract/provide (ls/proc b+p f init [max-each 1000]
                                  #:delimiter [delimiter #f])
  ((string?
    (any/c (listof xexpr?) . -> . any/c)
    any/c)
   ((and/c integer? (between/c 1 1000))
    #:delimiter (or/c #f string?))
   . ->* . any/c)
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
    (define-values (_ __ uri)
      (bucket+path->bucket&path&uri (string-append b "/?" qp)))
    (define h (make-date+authorization-headers
               (string-append b "/") "GET" '()))
    (define xpr (call/input-request "1.1" "GET" uri  h
                                    (lambda (in h)
                                      (check-response in h)
                                      (read-entity/xexpr in h))))
    (define contents (tags xpr 'Contents))
    (define prefixes (if delimiter
                         (tags xpr 'CommonPrefixes)
                         null))
    (define truncated? (equal? "true" (first-tag-value xpr 'IsTruncated)))
    (define next-marker (and truncated?
                             (if delimiter
                                 (first-tag-value xpr 'NextMarker)
                                 (first-tag-value (last contents) 'Key))))
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
  (string? . -> . (listof string?))
  (map (lambda (x) (first-tag-value x 'Key))
       (ls/proc b+p append '())))

;; TO-DO: Currently, for each object we make two HTTP requests and use
;; a separate HTTP connection for each request. Instead, it might be
;; nice to open just one connection, and make all the requests over
;; it. Unfortunately I think it still must be two requests for each
;; object, because a HEAD on the object will return different
;; information such as Content-Length than the response headers for
;; GET "?acl" request. But doing the requests over one persistent
;; connection would at least eliminate the connection overhead.
(define/contract/provide (ll* b+p)
  (string? . -> . (listof (list/c xexpr? string? xexpr?)))
  (define-values (b p) (bucket+path->bucket&path b+p))
  (for/list ([x (in-list (ls/proc b+p append '()))])
    (define path (string-append b "/" (first-tag-value x 'Key)))
    (list x (head path) (get-acl path))))

(define/contract/provide (ll b+p)
  (string? . -> . (listof (list/c string? string? xexpr?)))
  (for/list ([x (in-list (ll* b+p))])
    (match-define (list contents heads acl) x)
    (list (first-tag-value contents 'Key) heads acl)))

(define/contract/provide (copy b+p/from b+p/to)
  (string? string? . -> . string?)
  (define-values (u h)
    (uri&headers b+p/to
                 "PUT"
                 (hash 'x-amz-copy-source
                       (string-append "/" (uri-encode b+p/from)))))
  (call/output-request "1.1" "PUT" u
                       (lambda (out) (void))
                       0
                       h
                       (lambda (in h)
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
                         (define e (read-entity/bytes in h))
                         (match e
                           [(regexp "<Error>.*?</Error>")
                            (raise (header&response->exn:fail:aws
                                    h e (current-continuation-marks)))]
                           [_ (void)])
                         h)))

;; Our `end' arg is EXclusive
(define/contract (maybe-add-range-header heads beg end)
  (dict?
   (or/c #f exact-nonnegative-integer?)
   (or/c #f exact-nonnegative-integer?)
   . -> . dict?)
  (cond
   [(and beg end (< beg end))
    (maybe-dict-set heads
                    'Range
                    (format "bytes=~a-~a"
                            beg
                            (sub1 end)))] ;Range end is INclusive
   [else heads]))

(define/contract/provide (get/proc bucket+path
                                   reader
                                   [heads '()]
                                   [beg #f]
                                   [end #f])
  ((string? (input-port? string? . -> . any/c))
   (dict?
    (or/c #f exact-nonnegative-integer?)
    (or/c #f exact-nonnegative-integer?))
   . ->* . any/c)
  (define-values (u h) (uri&headers bucket+path
                                    "GET"
                                    (maybe-add-range-header heads beg end)))
  (call/input-request "1.1" "GET" u h
                      (lambda (in h)
                        (check-response in h)
                        (reader in h))))

(define/contract/provide (get/bytes bucket+path
                                    [heads '()]
                                    [beg #f]
                                    [end #f])
  ((string?)
   (dict?
    (or/c #f exact-nonnegative-integer?)
    (or/c #f exact-nonnegative-integer?))
   . ->* . bytes?)
  (get/proc bucket+path read-entity/bytes heads beg end))

(define/contract/provide (get/file bucket+path
                                   path
                                   [heads '()]
                                   #:mode [mode-flag 'binary]
                                   #:exists [exists-flag 'error])
  ((string? path?)
   (dict?
    #:mode (or/c 'binary 'text)
    #:exists (or/c 'error 'append 'update 'replace 'truncate
                   'truncate/replace))
   . ->* . void)
  (get/proc bucket+path
            (lambda (in h)
              (call-with-output-file* path
                                      (lambda (out)
                                        (read-entity/port in h out)
                                        (void))
                                      #:mode mode-flag
                                      #:exists exists-flag))
            heads))

(define 100MB (* 100 1024 1024))

(define/contract/provide (put bucket+path
                              writer
                              data-len
                              mime-type
                              [heads '()])
  ((string?
    (output-port? . -> . void?)
    (or/c #f exact-nonnegative-integer?)
    string?)
   (dict?)
   . ->* . string?)
  (when (> data-len 100MB)
    (log-aws-warning (tr "S3 `put' where " data-len
                         "(when > 100 MB, consider using `multipart-put')")))
  (define-values (u h)
    (uri&headers bucket+path
                 "PUT"
                 (dict-set* heads
                            'Content-Type mime-type
                            'Expect "100-continue")))
  (call/output-request "1.1" "PUT" u writer data-len h check-response))

(define/contract/provide (put/bytes bucket+path
                                    data
                                    mime-type
                                    [heads '()])
  ((string? bytes? string?) (dict?) . ->* . string?)
  (put bucket+path
       (lambda (out) (display data out))
       (bytes-length data)
       mime-type
       (dict-set heads
                 'Content-MD5 (bytes->Content-MD5 data))))

(define/contract/provide (put/file bucket+path
                                   path
                                   #:mime-type [mime-type #f]
                                   #:mode [mode-flag 'binary])
  ((string? path?)
   (#:mime-type (or/c #f string?) #:mode (or/c 'binary 'text))
   . ->* . string?)
  (put bucket+path
       (lambda (out)
         (call-with-input-file* path
                                (lambda (in)
                                  (copy-port in out))
                                #:mode mode-flag))
       (file-size path)
       (or mime-type ((path->mime-proc) path))
       (hash 'Content-MD5 (file->Content-MD5 path)
             'Content-Disposition (path->Content-Disposition path))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define 5MB (* 5 1024 1024)) ;multipart upload parts must be at least 5 MB
(define part-number/c (and/c exact-integer? (between/c 1 10000)))

(define/contract/provide (multipart-put b+p
                                        num-parts
                                        get-part
                                        [mime-type default-mime-type]
                                        [heads '()])
  ((string?
    exact-positive-integer?
    (exact-nonnegative-integer? . -> . bytes?))
   (string?
    dict?)
   . ->* . string?)
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
      (lambda (pool)
        (for/list ([n (in-range num-parts)])
          (add-job pool
                   (lambda ()
                     (upload-part b+p upid (add1 n) (get-part n)))))
        (get-results pool num-parts))))
  (complete-multipart-upload b+p upid parts)
  upid)

(define/contract/provide (multipart-put/file bucket+path
                                             path
                                             #:mime-type [mime-type #f]
                                             #:mode [mode-flag 'binary])
  ((string? path?)
   (#:mime-type (or/c #f string?) #:mode (or/c 'binary 'text))
   . ->* . string?)
  (define part-size 5MB)
  (define num-parts (ceiling (/ (file-size path) part-size)))
  (define (get-part part-num)
    ;; Each get-part does its own file open, so we're OK to
    ;; position/read the same file from multiple threads.
    (define (read-part in)
      (file-position in (* part-num part-size))
      (read-bytes part-size in))
    (call-with-input-file* path read-part #:mode mode-flag))
  (multipart-put bucket+path
                 num-parts
                 get-part
                 (or mime-type ((path->mime-proc) path))
                 (hash 'Content-Disposition (path->Content-Disposition path))))

(define/contract/provide (initiate-multipart-upload bucket+path mime-type heads)
  (string? string? dict? . -> . string?)
  (define b+p (string-append bucket+path "?uploads"))
  (define-values (u h) (uri&headers b+p
                                    "POST"
                                    (dict-set* heads 'Content-Type mime-type)))
  (define x (call/input-request "1.1" "POST" u h
                                (lambda (in h)
                                  (check-response in h)
                                  (read-entity/xexpr in h))))
  (define upid (first-tag-value x 'UploadId))
  (log-aws-debug (tr "initiate-multipart-upload returned" upid))
  upid)

(define/contract/provide (upload-part bucket+path upid part bstr)
  (string? string? part-number/c bytes?
           . -> . (cons/c part-number/c string?))
  (log-aws-debug (tr "upload-part start" upid part))
  (define b+p (string-append bucket+path
                             "?partNumber=" (number->string part)
                             "&uploadId=" upid))
  (define-values (u h)
    (uri&headers b+p
                 "PUT"
                 (hash 'Expect "100-continue"
                       'Content-MD5 (bytes->Content-MD5 bstr))))
  (call/output-request "1.1" "PUT" u bstr (bytes-length bstr) h
                       (lambda (in h)
                         (check-response in h)
                         (read-entity/bytes in h)
                         (define part-id (extract-field "ETag" h))
                         (log-aws-debug (tr "upload-part response" part-id))
                         (cons part part-id))))

(define/contract/provide (complete-multipart-upload bucket+path upid parts)
  (string? string? (listof (cons/c part-number/c string?)) . -> . xexpr?)
  (log-aws-debug (tr "complete-multipart-upload" upid parts))
  (define xm (parts->xml-bytes parts))
  (define b+p (string-append bucket+path "?uploadId=" upid))
  (define-values (u h) (uri&headers b+p "POST" '()))
  (call/output-request "1.1" "POST" u xm (bytes-length xm) h
                       (lambda (in h)
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
  ((listof (cons/c part-number/c string?)) . -> . bytes?)
  (string->bytes/utf-8
   (xexpr->string
    `(CompleteMultipartUpload
      ()
      ,@(map (lambda (x)
               (match-define (cons part etag) x)
               (let ([part (number->string part)]
                     [etag (cadr (regexp-match #rx"^\"(.+?)\"$" etag))])
                 `(Part ()
                        (PartNumber () ,part)
                        (ETag () ,etag))))
             (sort parts < #:key car))))))

(define/contract/provide (list-multipart-uploads bucket)
  (string? . -> . xexpr?)
  (define b+p (string-append bucket "/?uploads"))
  (define-values (u h) (uri&headers b+p "GET" '()))
  (call/input-request "1.1" "GET" u h
                      (lambda (in h)
                        (check-response in h)
                        (read-entity/xexpr in h))))

(define/contract/provide (abort-multipart-upload bucket+path upid)
  (string? string? . -> . void)
  (define b+p (string-append bucket+path"?uploadId=" upid))
  (define-values (u h) (uri&headers b+p "DELETE" '()))
  (call/input-request "1.1" "DELETE" u h
                      (lambda (in h)
                        (check-response in h)
                        h)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define/contract (path->Content-Disposition path)
  (path-string? . -> . string?)
  (define-values (base name dir?) (split-path path))
  (format "attachment; filename=\"~a\"" name))

;; ETag and Content-MD5 utils

(define/contract (port->Content-MD5 in)
  (input-port? . -> . string?)
  (match (base64-encode (md5 in #f)) ;zap trailing \r\n
    [(regexp "^(.+?)\r\n$" (list _ s))
     (bytes->string/utf-8 s)]))

(define/contract (bytes->Content-MD5 b)
  (bytes? . -> . string?)
  (port->Content-MD5 (open-input-bytes b)))

(define (file->Content-MD5 p)
  (path-string? . -> . string?)
  (call-with-input-file* p port->Content-MD5))

;; Query string request authentication
(define/contract/provide (sign-uri bucket+path method expires heads)
  (string? string? exact-positive-integer? dict?
           . -> . string?)
  (define s (canonical-string-to-sign bucket+path method
                                      (number->string expires) heads))
  (define-values (b p) (bucket+path->bucket&path bucket+path))
  (string-append
   (bucket&path->uri b p)
   "?"
   (dict->form-urlencoded
    `((AWSAccessKeyId ,(public-key))
      (Expires ,(number->string expires))
      (Signature ,(sha1-encode s))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(define (guess-mime-type path)
  ;; Exception handler in case `split-path' barfs. Since need for
  ;; that, use to avoid else case for `match', too.
  (with-handlers ([exn:fail? (lambda (exn) default-mime-type)])
    (define-values (base name dir?) (split-path path))
    (match (path-element->string name)
      [(regexp "\\.([^.]*)$" (list _ ext))
       (hash-ref ext->mime ext default-mime-type)])))

;; Let users replace.
(define path->mime-proc (make-parameter guess-mime-type))
(provide path->mime-proc)

(define default-mime-type "application/x-unknown-content-type")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test

(module+ test
  (require rackunit net/url "tests/data.rkt")
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
   "canonical-amz-headers-string"
   (check-equal? (canonical-amz-headers-string
                  (hash 'X-AMZ-Meta-ZZZ "VALUE"
                        'X-Amz-Meta-x "A\nZ"
                        'X-Amz-Meta-AAA "Value"
                        'IGNORE-ME "PLEASE"))
                 (string-append "x-amz-meta-aaa:Value" "\n"
                                "x-amz-meta-x:A,Z" "\n"
                                "x-amz-meta-zzz:VALUE" "\n")))

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

  ;; ------------------------------------------------------------
  (define/contract (test-bucket-ops region)
    ((or/c #f string?) . -> . any)

    (printf "~a: Endpoint=~a Region=~a\n"
            (if (s3-path-requests?) "Path Style" "Virtual Hosted")
            (s3-host)
            region)

    (define bucket (string-append (test/bucket) "-" (or region "us-standard")))

    (ensure-have-keys)

    (define (member? x xs)
      (not (not (member x xs))))

    (create-bucket bucket region)
    (check-true (member? bucket (list-buckets)))

    (check-equal? (ls (string-append bucket "/")) '())
    (check-equal? (ls/proc (string-append bucket "/") (lambda (v x) #t) #f) #f)

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
    (check-equal? (ls/proc (string-append bucket "/") (lambda (v x) (add1 v)) 0) 1)
    (let loop ([p (string-split (test/path) "/")] [prefix ""])
      (define more? (pair? (cdr p)))
      (ls/proc (string-append bucket "/" prefix)
               #:delimiter "/"
               (lambda (v xs)
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
    (check-equal?
     (call/input-request "1.1" "GET"
                         (sign-uri b+p "GET" (+ (current-seconds) 60) '())
                         '()
                         read-entity/bytes)
     data)

    ;; ACL
    (define acl (get-acl b+p))
    (put-acl b+p acl)
    (check-equal? (get-acl b+p) acl)

    ;; Copy
    (define b+p/copy (string-append b+p "-copy"))
    (copy b+p b+p/copy)
    (check-true (member? (string-append (test/path) "-copy")
                         (ls b+p/copy)))

    ;; Try put & get file, both simple and multipart
    (define (put&get-file put-using p)
      (define chksum (file->Content-MD5 p))
      (put-using b+p p #:mime-type "text/plain")
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
    (define p (build-path (find-system-path 'temp-dir) "s3-test-file.txt"))
    (with-output-to-file p
      (lambda () (for ([i (in-range 10000)]) (displayln (random))))
      #:exists 'replace #:mode 'text)
    (put&get-file put/file p)
    (put&get-file multipart-put/file p)

    ;; Multipart upload: Do with enough parts to exercise the worker
    ;; pool of 4 threads. How about 8 parts.
    (define part-size 5MB) ;minimum S3 will accept for multipart parts
    (define (get-part-bytes n) (make-bytes part-size n))
    (define num-parts 8)
    (multipart-put b+p num-parts get-part-bytes)
    (for ([i (in-range num-parts)])
      ;; This is also an opportunity to test Range requests ability:
      (check-equal? (get/bytes b+p '() (* i part-size) (* (add1 i) part-size))
                    (get-part-bytes i)))

    ;; Cleanup
    (delete b+p/copy)
    (delete b+p)
    (delete-bucket bucket)
    (void))

  (for ([pool '(0 10)])
    (printf "=== Test using connection pool timeout of ~a sec ===\n" pool)
    (parameterize ([current-pool-timeout pool])
      (parameterize ([s3-path-requests? #f])
        (parameterize ([s3-host "s3.amazonaws.com"])
          (test-bucket-ops #f) ;us-standard
          (test-bucket-ops "eu-west-1"))
        (parameterize ([s3-host "s3-eu-west-1.amazonaws.com"])
          (test-bucket-ops "eu-west-1")))
      (parameterize ([s3-path-requests? #t])
        (parameterize ([s3-host "s3.amazonaws.com"])
          (test-bucket-ops #f)) ;us-standard
        (parameterize ([s3-host "s3-eu-west-1.amazonaws.com"])
          (test-bucket-ops "eu-west-1")))))

  ;; ------------------------------------------------------------

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
    (lambda ()
      (put (string-append (test/bucket) "/" (test/path))
           (lambda (out)
             ;; Set flag that we were asked to write something.
             ;; (But for this test, don't actually write anything.)
             (set! writer-called? #t))
           ;; Content-Length that should elicit 400 error
           (expt 2 64) ;16,384 petabytes might be too large
           "text/plain")))
   (check-false writer-called?)
   (delete-bucket (test/bucket))
   (void)))
