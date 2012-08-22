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
                  'x-amz-sha256-tree-hash (bytes->hex-string (tree-hash data))
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
  (for/or ([e (in-range 20 33 1)])      ;1MB to 4GB in powers of 2
      (equal? n (expt 2 e))))

(define/contract (start-multipart-upload name part-size desc)
  (string? exact-positive-integer? string?  . -> . string?)
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
                  'x-amz-sha256-tree-hash (bytes->hex-string (tree-hash data))
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
  (string? string? exact-positive-integer? bytes? . -> . any/c)
  (define id (start-multipart-upload name part-size desc))
  (printf "upload-id ~a\n" id)
  (define len (bytes-length data))
  (printf "part-size ~a total-len ~a\n" part-size len)
  (for ([i (in-range 0 len part-size)])
      (upload-part name id i (subbytes data i (min (+ i part-size) len))))
  (finish-multipart-upload name id len (bytes->hex-string (tree-hash data))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (read-entity/jsexpr in h)
  (bytes->jsexpr (read-entity/bytes in h)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define/contract (tree-hash d)
  (bytes? . -> . bytes?)
  (define (data-blocks d)
    (define total-len (bytes-length d))
    (for/list ([i (in-range 0 total-len 1MB)])
        (SHA256 (subbytes d i (min (+ i 1MB) total-len)))))
  (reduce-pairs (lambda (a b)
                  (if b
                      (SHA256 (bytes-append a b))
                      a))
                (data-blocks d)
                #f))

;;(bytes->hex-string (tree-hash #"hi"))
;;(tree-hash (make-bytes (+ (* 3 1MB) 4)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (create-vault "test")
;; (list-vaults)
;; (delete-vault "test")
;; (list-vaults)
;; (define id (create-archive "test" #"Hello, world"))
;; (delete-archive "test" id)
;; (create-archive "test" (make-bytes (+ 3 (* 4 1MB))))
;; (create-archive/multipart-upload "test" "desc" 1MB (make-bytes (+ 3 (* 4 1MB))))
