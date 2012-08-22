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
(define region (make-parameter "us-east-1"))
(define host (string-append service "." (region) ".amazonaws.com"))

(define (create-vault name)
  (define m "PUT")
  (define u (string-append "http://" host "/-/vaults/" name))
  (define h (hash 'Host host
                  'Date (seconds->gmt-8601-string 'basic (current-seconds))
                  'x-amz-glacier-version "2012-06-01"
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
                  'x-amz-glacier-version "2012-06-01"
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
                  'x-amz-glacier-version "2012-06-01"
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
                  'x-amz-glacier-version "2012-06-01"
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
(define/contract (create-archive name data [desc ""])
  ((string? bytes?) (string?)  . ->* . string?)
  (define m "POST")
  (define u (string-append "http://" host "/-/vaults/" name "/archives"))
  (define h (hash 'Host host
                  'Date (seconds->gmt-8601-string 'basic (current-seconds))
                  'Content-Length (bytes-length data)
                  'x-amz-content-sha256 (bytes->hex-string (SHA256 data))
                  'x-amz-sha256-tree-hash (bytes->hex-string (tree-hash data))
                  'x-amz-glacier-version "2012-06-01"
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
                         (read-entity/jsexpr p h)
                         (extract-field "x-amz-archive-id" h))))

(define (delete-archive vault archive-id)
  (define m "DELETE")
  (define u (string-append "http://" host "/-/vaults/" vault
                           "/archives/" archive-id))
  (define h (hash 'Host host
                  'Date (seconds->gmt-8601-string 'basic (current-seconds))
                  'x-amz-glacier-version "2012-06-01"
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

(define (read-entity/jsexpr in h)
  (bytes->jsexpr (read-entity/bytes in h)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define 1MB (* 1024 1024))
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
