#lang racket

(require net/head
         net/base64
         net/uri-codec
         file/md5
         xml
         (planet gh/http/request)
         (planet gh/http/head)
         "util.rkt"
         "exn.rkt"
         "keys.rkt"
         )

(define s3-scheme (make-parameter "http"))
(define s3-host (make-parameter "s3.amazonaws.com"))
(provide s3-scheme s3-host)

(define/contract/provide (bucket&path->uri b p)
  (string? string? . -> . string?)
  (string-append (s3-scheme) "://" b "." (s3-host) "/" p))

(define/contract/provide (bucket+path->bucket&path b+p)
  (string? . -> . (values string? string?))
  (match b+p
    [(regexp "^([^/]+?)/(.*?)$" (list _ b p)) (values b (or p ""))]
    [(regexp "^/") (error 'bucket+path->bucket&path "don't use leading /")]
    [else (error 'bucket+path->bucket&path "invalid bucket+path")]))

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
         [xs (sort xs string<?)])                             ;2
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

(define/contract/provide (create-bucket b)
  (string? . -> . string?)
  (put/bytes (string-append b "/") #"" ""))

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
  (call/input-request "1.1" "DELETE" u h (lambda (in h) h)))

(define/contract/provide (head bucket+path)
  (string? . -> . string?)
  (define-values (u h) (uri&headers bucket+path "HEAD" '()))
  (call/input-request "1.1" "HEAD" u h (lambda (in h) h)))

(define/contract/provide (get-acl bucket+path [heads '()])
  ((string?) (dict?) . ->* . xexpr?)
  (define b+p (string-append bucket+path "?acl"))
  (get/proc b+p
            read-entity/xexpr
            heads))

(define/contract/provide (ls b+p)
  (string? . -> . (listof string?))
  (define-values (b p u) (bucket+path->bucket&path&uri b+p))
  ;; Ignore the path `p' when making the authorization header
  (define h (make-date+authorization-headers (string-append b "/") "GET" '()))
  ;; Will return only ~1000 keys at a time, so loop
  (let loop ([marker ""]
             [xs '()])
    (define uri (string-append
                 (s3-scheme) "://" b "." (s3-host) "/" "?"
                 (dict->form-urlencoded
                  `((prefix ,p)
                    (marker ,marker)
                    (max-keys "1000"))) ))
    (define xpr (call/input-request "1.1" "GET" uri h
                                    (lambda (in h)
                                      (check-response in h)
                                      (read-entity/xexpr in h))))
    (define keys (map third (tags xpr 'Key)))
    (if (empty? keys)
        xs
        (loop (last keys)
              (append xs keys)))))

(define/contract/provide (ll b+p)
  (string? . -> . (listof (list/c string? string? xexpr?)))
  (define xs (ls b+p))
  (define-values (b p) (bucket+path->bucket&path b+p))
  ;; TO-DO: Currently, for each object we make two HTTP requests and
  ;; use a separate HTTP connection for each request. Instead, it
  ;; might be nice to open just one connection, and make all the
  ;; requests over it. Unfortunately I think it still must be two
  ;; requests for each object, because a HEAD on the object will
  ;; return different information such as Content-Length than the
  ;; response headers for GET "?acl" request. But doing the requests
  ;; over one persistent connection would at least eliminate the
  ;; connection overhead.
  (for/list ([x (in-list xs)])
      (define p (string-append b "/" x))
    (define heads (head p))
    (define acl (get-acl p))
    (list x heads acl)))

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
                           [else (void)])
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
  (get/proc bucket+path
            (lambda (in h)
              (define b (read-entity/bytes in h))
              (unless (and beg end) ;range
                (unless (port-matches-etag? h (open-input-bytes b))
                  (error 'get/bytes
                         "MD5 checksum did not match ETag")))
              b)
            heads
            beg
            end))

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
   . ->* . any/c)
  (get/proc bucket+path
            (lambda (in h)
              (call-with-output-file* path
                                      (lambda (out)
                                        (read-entity/port in h out)
                                        (void))
                                      #:mode mode-flag
                                      #:exists exists-flag)
              (if (file-matches-etag? h path)
                  #t
                  (error 'get/file
                         "MD5 checksum did not match ETag")))
            heads))

(define/contract/provide (put bucket+path
                              writer
                              data-len
                              mime-type
                              reader
                              [heads '()])
  ((string?
    (output-port? . -> . void?)
    (or/c #f exact-nonnegative-integer?)
    string?
    (input-port? string? . -> . any/c))
   (dict?)
   . ->* . string?)
  (define-values (u h)
    (uri&headers bucket+path
                 "PUT"
                 (dict-set* heads
                            'Content-Type mime-type
                            'Expect "100-continue")))
  (call/output-request "1.1" "PUT" u writer data-len h
                       (lambda (in h)
                         (check-response in h)
                         (reader in h)
                         h)))

(define/contract/provide (put/bytes bucket+path
                                    data
                                    mime-type
                                    [heads '()])
  ((string? bytes? string?) (dict?) . ->* . string?)
  (put bucket+path
       (lambda (out) (display data out))
       (bytes-length data)
       mime-type
       read-entity/bytes
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
       read-entity/bytes
       (hash 'Content-MD5 (file->Content-MD5 path)
             'Content-Disposition (path->Content-Disposition path))))

(define/contract (path->Content-Disposition path)
  (path-string? . -> . string?)
  (define-values (base name dir?) (split-path path))
  (format "attachment; filename=\"~a\"" name))

;;; ETag and Content-MD5 utils

(define (port-matches-etag? h in)
  (match (extract-field "ETag" h)
    [#f #t]
    [(regexp "^\"(.+?)\"$" (list _ etag/header)) ;zap quotes
     (define etag/port (bytes->string/utf-8 (md5 in #t)))
     (string=? etag/header etag/port)]))

(define (file-matches-etag? h f)
  (call-with-input-file* f
                         (lambda (in)
                           (port-matches-etag? h in))))

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
  (require "run-suite.rkt"
           net/url)

  (def/run-test-suite
   (test-case
    "bucket+path->bucket&path&uri"
    (define-values (b p u) (bucket+path->bucket&path&uri "bucket/path/name"))
    (check-equal? b "bucket")
    (check-equal? p "path/name")
    (check-equal? u "http://bucket.s3.amazonaws.com/path/name"))

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

   (test-case
    "put, get, head, ls"
    (ensure-have-keys)

    (define (member? x xs)
      (not (not (member x xs))))

    (create-bucket (test/bucket))
    (check-true (member? (test/bucket) (list-buckets)))

    (define b+p (string-append (test/bucket) "/" (test/path)))

    (define data #"Hello, world.")
    (put/bytes b+p data default-mime-type)
    (check-equal? (get/bytes b+p) data)
    (check-equal? (get/bytes b+p '() 0 4)
                  (subbytes data 0 4))
    (check-equal? 200 (extract-http-code (head b+p)))
    (check-true (xexpr? (get-acl b+p)))

    (check-equal? (call/input-url
                   (string->url (sign-uri b+p "GET" (+ (current-seconds) 60) '()))
                   get-pure-port
                   (lambda (in)
                     (port->bytes in)))
                  data)

    (define p (build-path 'same "tests" "s3-test-file-to-get-and-put.txt"))
    (define chksum (file->Content-MD5 p))
    (put/file b+p p #:mime-type "text/plain")
    (get/file b+p p #:exists 'replace)
    (check-equal? (file->Content-MD5 p) chksum)
    (check-equal? 200 (extract-http-code (head b+p)))
    (check-true (member? (test/path) (ls b+p)))
    (check-true (member? (test/path)
                         (ls (string-append (test/bucket) "/"))))
    (check-true (member? (test/path)
                         (ls (string-append (test/bucket)
                                            "/"
                                            (substring (test/path) 0 3)))))

    (define b+p/copy (string-append b+p "-copy"))
    (copy b+p b+p/copy)
    (check-true (member? (string-append (test/path) "-copy")
                         (ls b+p/copy)))

    ;; Cleanup
    (delete b+p/copy)
    (delete b+p)
    (delete-bucket (test/bucket))
    (void))

   (test-case
    "100-continue"
    ;; Confirm that the gh/http collection's handling of 100-continue is
    ;; working as expected with S3.
    ;;
    ;; Make a PUT request with a Content-Length vastly bigger than S3
    ;; will accept. Check that S3 responds as expected with "400 Bad
    ;; Request". Then check that our `writer' proc was NOT called. (Don't
    ;; worry, if it is called, we don't actually write anything, much
    ;; less the huge number of bytes.)
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
            "text/plain"
            (lambda (in h)
              (check-response in h) ;Should raise exn:fail:aws due to 400
              (port->bytes in)))))
    (check-false writer-called?)
    (delete-bucket (test/bucket))
    (void))
   ))
