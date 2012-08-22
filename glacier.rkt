#lang racket

(require (planet gh/http)
         json
         file/sha1
         net/head
         "exn.rkt"
         "hmac-sha256.rkt"
         "keys.rkt"
         "util.rkt"
         "sigv4.rkt"
         )

(provide region
         create-vault
         delete-vault
         list-vaults
         describe-vault
         create-archive
         create-archive/multipart-upload
         create-archive-from-file
         valid-part-size?
         )

(define service "glacier")
(define glacier-version "2012-06-01")
(define region (make-parameter "us-east-1"))
(define host (string-append service "." (region) ".amazonaws.com"))

(define 1MB (* 1024 1024))

(define (create-vault name)
  (define m "PUT")
  (define u (string-append "http://" host "/-/vaults/" name))
  (define h (hash 'Host host
                  'Date (seconds->gmt-8601-string 'basic (current-seconds))
                  'x-amz-glacier-version glacier-version
                  ))
  (call/input-request "1.1"
                      m
                      u
                      (dict-set h
                                'Authorization
                                (aws-v4-authorization
                                 m
                                 u
                                 h
                                 #""
                                 (region)
                                 service))
                      (lambda (p h)
                        (check-response p h)
                        (void (read-entity/bytes p h))
                        #t)))

(define (delete-vault name)
  (define m "DELETE")
  (define u (string-append "http://" host "/-/vaults/" name))
  (define h (hash 'Host host
                  'Date (seconds->gmt-8601-string 'basic (current-seconds))
                  'x-amz-glacier-version glacier-version
                  ))
  (call/input-request "1.1"
                      m
                      u
                      (dict-set h
                                'Authorization
                                (aws-v4-authorization
                                 m
                                 u
                                 h
                                 #""
                                 (region)
                                 service))
                      (lambda (p h)
                        (check-response p h)
                        (void (read-entity/bytes p h))
                        #t)))

;; TO-DO: Handle >1,000 vaults with multi requests
;; TO-DO: Return a list of structs instead of hasheq?
(define (list-vaults)
  (define m "GET")
  (define u (string-append "http://" host "/-/vaults/"))
  (define h (hash 'Host host
                  'Date (seconds->gmt-8601-string 'basic (current-seconds))
                  'x-amz-glacier-version glacier-version
                  ))
  (call/input-request "1.1"
                      m
                      u
                      (dict-set h
                                'Authorization
                                (aws-v4-authorization
                                 m
                                 u
                                 h
                                 #""
                                 (region)
                                 service))
                      (lambda (p h)
                        (check-response p h)
                        (hash-ref (read-entity/jsexpr p h) 'VaultList))))

(define (describe-vault name)
  (define m "GET")
  (define u (string-append "http://" host "/-/vaults/" name))
  (define h (hash 'Host host
                  'Date (seconds->gmt-8601-string 'basic (current-seconds))
                  'x-amz-glacier-version glacier-version
                  ))
  (call/input-request "1.1"
                      m
                      u
                      (dict-set h
                                'Authorization
                                (aws-v4-authorization
                                 m
                                 u
                                 h
                                 #""
                                 (region)
                                 service))
                      (lambda (p h)
                        (check-response p h)
                        (read-entity/jsexpr p h))))

;; Return the x-amz-archive-id
(define/contract (create-archive name data desc)
  (string? string? bytes?  . -> . string?)
  (define m "POST")
  (define u (string-append "http://" host "/-/vaults/" name "/archives"))
  (define h (hash 'Host host
                  'Date (seconds->gmt-8601-string 'basic (current-seconds))
                  'Content-Length (bytes-length data)
                  'x-amz-glacier-version glacier-version
                  'x-amz-content-sha256 (bytes->hex-string (SHA256 data))
                  'x-amz-sha256-tree-hash (tree-hash data)
                  'x-amz-archive-description desc
                  ))
  (call/output-request "1.1"
                       m
                       u
                       (lambda (out) (display data out))
                       (bytes-length data)
                       (dict-set h
                                 'Authorization
                                 (aws-v4-authorization
                                  m
                                  u
                                  h
                                  data
                                  (region)
                                  service))
                       (lambda (p h)
                         (check-response p h)
                         (void (read-entity/jsexpr p h))
                         (extract-field "x-amz-archive-id" h))))

(define (delete-archive vault archive-id)
  (define m "DELETE")
  (define u (string-append "http://" host "/-/vaults/" vault
                           "/archives/" archive-id))
  (define h (hash 'Host host
                  'Date (seconds->gmt-8601-string 'basic (current-seconds))
                  'x-amz-glacier-version glacier-version
                  ))
  (call/input-request "1.1"
                      m
                      u
                      (dict-set h
                                'Authorization
                                (aws-v4-authorization
                                 m
                                 u
                                 h
                                 #""
                                 (region)
                                 service))
                      (lambda (p h)
                        (check-response p h)
                        (void (read-entity/bytes p h))
                        #t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (valid-part-size? n)
  (and (exact-integer? n)
       (for/or ([e (in-range 20 33 1)])      ;1MB to 4GB in powers of 2
           (equal? n (expt 2 e)))))

(define/contract (start-multipart-upload name part-size desc)
  (string? valid-part-size? string?  . -> . string?)
  (define m "POST")
  (define u (string-append "http://" host "/-/vaults/" name "/multipart-uploads"))
  (define h (hash 'Host host
                  'Date (seconds->gmt-8601-string 'basic (current-seconds))
                  'x-amz-glacier-version glacier-version
                  'x-amz-part-size part-size
                  'x-amz-archive-description desc
                  ))
  (call/output-request "1.1"
                       m
                       u
                       (lambda (out) (void))
                       0
                       (dict-set h
                                 'Authorization
                                 (aws-v4-authorization
                                  m
                                  u
                                  h
                                  #""
                                  (region)
                                  service))
                       (lambda (p h)
                         (check-response p h)
                         (void (read-entity/bytes p h))
                         (extract-field "x-amz-multipart-upload-id" h))))


;; Note: (bytes-length data) must be *exactly* the part-size for every
;; part but the last, where it must be <=.
;;
;; Returns #t unless it throws an exception.
(define/contract (upload-part name upload-id offset data)
  (string? string? exact-nonnegative-integer? bytes?  . -> . void)
  (printf "upload-part ~a-~a\n" offset (+ offset (bytes-length data)))
  (define m "PUT")
  (define u (string-append "http://" host "/-/vaults/" name
                           "/multipart-uploads/" upload-id))
  (define h (hash 'Host host
                  'Date (seconds->gmt-8601-string 'basic (current-seconds))
                  'x-amz-glacier-version glacier-version
                  'x-amz-part-size 1MB
                  'Content-Length (bytes-length data)
                  'Content-Type "application/octet-stream"
                  'Content-Range (format "bytes ~a-~a/*"
                                         offset
                                         (sub1 (+ offset (bytes-length data))))
                  'x-amz-content-sha256 (bytes->hex-string (SHA256 data))
                  'x-amz-sha256-tree-hash (tree-hash data)
                  ))
  (call/output-request "1.1"
                       m
                       u
                       (lambda (out) (display data out))
                       (bytes-length data)
                       (dict-set h
                                 'Authorization
                                 (aws-v4-authorization
                                  m
                                  u
                                  h
                                  data
                                  (region)
                                  service))
                       (lambda (p h)
                         (check-response p h)
                         (void))))

(define/contract (finish-multipart-upload name upload-id total-size tree-hash)
  (string? string? exact-nonnegative-integer? string?  . -> . string?)
  (define m "POST")
  (define u (string-append "http://" host "/-/vaults/" name
                           "/multipart-uploads/" upload-id))
  (define h (hash 'Host host
                  'Date (seconds->gmt-8601-string 'basic (current-seconds))
                  'x-amz-glacier-version glacier-version
                  'x-amz-archive-size total-size
                  'x-amz-sha256-tree-hash tree-hash
                  ))
  (call/output-request "1.1"
                       m
                       u
                       (lambda (out) (void))
                       0
                       (dict-set h
                                 'Authorization
                                 (aws-v4-authorization
                                  m
                                  u
                                  h
                                  #""
                                  (region)
                                  service))
                       (lambda (p h)
                         (check-response p h)
                         (void (read-entity/bytes p h))
                         (extract-field "x-amz-archive-id" h))))

(define/contract (create-archive/multipart-upload name desc part-size data)
  (string? string? valid-part-size? bytes? . -> . string?)
  (define id (start-multipart-upload name part-size desc))
  ;;(printf "upload-id ~a\n" id)
  (define len (bytes-length data))
  ;;(printf "part-size ~a total-len ~a\n" part-size len)
  (for ([i (in-range 0 len part-size)])
      (upload-part name id i (subbytes data i (min (+ i part-size) len))))
  (finish-multipart-upload name id len (tree-hash data)))

(define/contract (create-archive-from-file vault path)
  (string? path? . -> . string?)
  (define ps (path->string path))
  (define len (file-size ps))
  (with-input-from-file ps
    (lambda ()
      (define desc (string-append ps " " (seconds->gmt-8601-string)))
      (define id (start-multipart-upload vault 1MB desc))
      (let loop ([i 0]
                 [xs '()])
        (define b (read-bytes 1MB))
        (cond
         [(eof-object? b)
          (finish-multipart-upload vault id len (hashes->tree (reverse xs)))]
         [else
          (upload-part vault id i b)
          (loop (+ i 1MB)
                (cons (SHA256 b) xs))])))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define/contract (request-inventory name desc [sns #f])
  ((string? string?) ((or/c #f string?)) . ->* . string?)
  (define data (jsexpr->bytes (apply hasheq
                                     (append
                                      (list 'Type "inventory-retrieval"
                                            'Description desc
                                            'Format "JSON")
                                      (if sns (list 'SNSTopic sns) '())))))
  (define m "POST")
  (define u (string-append "http://" host "/-/vaults/" name "/jobs"))
  (define h (hash 'Host host
                  'Date (seconds->gmt-8601-string 'basic (current-seconds))
                  'Content-Length (bytes-length data)
                  'x-amz-glacier-version glacier-version
                  ))
  (call/output-request "1.1"
                       m
                       u
                       (lambda (out) (display data out))
                       (bytes-length data)
                       (dict-set h
                                 'Authorization
                                 (aws-v4-authorization
                                  m
                                  u
                                  h
                                  data
                                  (region)
                                  service))
                       (lambda (p h)
                         (check-response p h)
                         (void (read-entity/bytes p h))
                         (extract-field "x-amz-job-id" h))))

(define (get-job-output name job)
  (define m "GET")
  (define u (string-append "http://" host "/-/vaults/" name "/jobs/" job
                           "/output"))
  (define h (hash 'Host host
                  'Date (seconds->gmt-8601-string 'basic (current-seconds))
                  'x-amz-glacier-version glacier-version
                  ))
  (call/input-request "1.1"
                      m
                      u
                      (dict-set h
                                'Authorization
                                (aws-v4-authorization
                                 m
                                 u
                                 h
                                 #""
                                 (region)
                                 service))
                      (lambda (p h)
                        (check-response p h)
                        (read-entity/jsexpr p h))))

(define (list-jobs name)
  (define m "GET")
  (define u (string-append "http://" host "/-/vaults/" name "/jobs"))
  (define h (hash 'Host host
                  'Date (seconds->gmt-8601-string 'basic (current-seconds))
                  'x-amz-glacier-version glacier-version
                  ))
  (call/input-request "1.1"
                      m
                      u
                      (dict-set h
                                'Authorization
                                (aws-v4-authorization
                                 m
                                 u
                                 h
                                 #""
                                 (region)
                                 service))
                      (lambda (p h)
                        (check-response p h)
                        (read-entity/jsexpr p h))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (read-entity/jsexpr in h)
  (bytes->jsexpr (read-entity/bytes in h)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Produce a list of SHA256 hashes of each 1MB of the bytes
(define (block-hashes d)
  (define total-len (bytes-length d))
  (for/list ([i (in-range 0 total-len 1MB)])
      (SHA256 (subbytes d i (min (+ i 1MB) total-len)))))

;; Given a list of data block hashes, "tree them up" for AWS Glacier
(define/contract (hashes->tree xs)
  ((listof bytes?) . -> . string?)
  (bytes->hex-string (reduce-pairs (lambda (a b)
                                     (if b
                                         (SHA256 (bytes-append a b))
                                         a))
                                   xs
                                   #f)))

(define/contract (tree-hash d)
  (bytes? . -> . string?)
  (hashes->tree (block-hashes d)))

;;(bytes->hex-string (tree-hash #"hi"))
;;(tree-hash (make-bytes (+ (* 3 1MB) 4)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Examples

;; (create-vault "test")
;; (list-vaults)
;; (delete-vault "test")
;; (list-vaults)
;; (define id (create-archive "test" #"Hello, world"))
;; (delete-archive "test" id)
;; (create-archive "test" (make-bytes (+ 3 (* 4 1MB))))
;; (create-archive/multipart-upload "test" "desc" 1MB (make-bytes (+ 3 (* 4 1MB))))
;; (create-archive-from-file "test" (build-path 'same "manual.scrbl"))
;; (request-inventory "test" "")
;; (for ([x (in-list (hash-ref (list-jobs "test") 'JobList '()))])
;;     (define id (hash-ref x 'JobId))
;;   (define date (hash-ref x 'CreationDate))
;;   (define completed? (hash-ref x 'Completed))
;;   (printf "Job ~a created ~a is ~a completed.\n"
;;           id date (if completed? "" "NOT"))
;;   (when completed?
;;     (displayln (get-job-output "test" id))))
