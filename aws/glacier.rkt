;; Copyright (c) 2012-2022 by Greg Hendershott.
;; SPDX-License-Identifier: BSD-2-Clause

#lang racket/base

(require http
         json
         net/head
         racket/contract/base
         racket/contract/region
         racket/dict
         racket/function
         racket/match
         sha
         "exn.rkt"
         "sigv4.rkt"
         "take.rkt"
         "util.rkt")

(provide region
         create-vault
         delete-vault
         list-vaults
         describe-vault
         create-archive
         create-archive/multipart-upload
         create-archive-from-file
         create-archive-from-port
         delete-archive
         valid-part-size?
         retrieve-inventory
         retrieve-archive
         list-jobs
         get-job-output
         get-job-output-to-file)

(define service "glacier")
(define glacier-version "2012-06-01")
(define region (make-parameter "us-east-1"))
(define (host) (string-append service "." (region) ".amazonaws.com"))
(define num-threads (make-parameter 8))

(define 1MB (* 1024 1024))

(define/contract (create-vault name)
  (string? . -> . (or/c #t exn:fail:aws?))
  (define m "PUT")
  (define u (string-append "http://" (host) "/-/vaults/" name))
  (define h (hasheq 'Host (host)
                    'Date (seconds->gmt-8601-string 'basic (current-seconds))
                    'x-amz-glacier-version glacier-version))
  (call/input-request "1.1"
                      m
                      u
                      (add-v4-auth-heads #:heads   h
                                         #:method  m
                                         #:uri     u
                                         #:sha256  (sha256-hex-string #"")
                                         #:region  (region)
                                         #:service service)
                      (λ (p h)
                        (check-response p h)
                        (void (read-entity/bytes p h))
                        #t)))

(define/contract (delete-vault name)
  (string? . -> . (or/c #t exn:fail:aws?))
  (define m "DELETE")
  (define u (string-append "http://" (host) "/-/vaults/" name))
  (define h (hasheq 'Host (host)
                    'Date (seconds->gmt-8601-string 'basic (current-seconds))
                    'x-amz-glacier-version glacier-version))
  (call/input-request "1.1"
                      m
                      u
                      (dict-set h
                                'Authorization
                                (aws-v4-authorization m u h
                                                      (sha256-hex-string #"")
                                                      (region) service))
                      (λ (p h)
                        (check-response p h)
                        (void (read-entity/bytes p h))
                        #t)))

;; TO-DO: Handle >1,000 vaults with multi requests
;; TO-DO: Return a list of structs instead of hasheq?
(define (list-vaults)
  (define m "GET")
  (define u (string-append "http://" (host) "/-/vaults/"))
  (define h (hasheq 'Host (host)
                    'Date (seconds->gmt-8601-string 'basic (current-seconds))
                    'x-amz-glacier-version glacier-version))
  (call/input-request "1.1"
                      m
                      u
                      (dict-set h
                                'Authorization
                                (aws-v4-authorization m u h
                                                      (sha256-hex-string #"")
                                                      (region) service))
                      (λ (p h)
                        (check-response p h)
                        (hash-ref (read-entity/jsexpr p h) 'VaultList))))

(define/contract (describe-vault name)
  (string? . -> . jsexpr?)
  (define m "GET")
  (define u (string-append "http://" (host) "/-/vaults/" name))
  (define h (hasheq 'Host (host)
                    'Date (seconds->gmt-8601-string 'basic (current-seconds))
                    'x-amz-glacier-version glacier-version))
  (call/input-request "1.1"
                      m
                      u
                      (dict-set h
                                'Authorization
                                (aws-v4-authorization m u h
                                                      (sha256-hex-string #"")
                                                      (region) service))
                      (λ (p h)
                        (check-response p h)
                        (read-entity/jsexpr p h))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define/contract (set-vault-notifications name sns inventory? archive?)
  (string? string? boolean? boolean? . -> . (or/c #t exn:fail:aws?))
  (unless (or inventory? archive?)
    (error 'set-vault-notifications
           "One of inventory? or archive? must be #t"))
  (define m "PUT")
  (define u (string-append "http://" (host) "/-/vaults/" name
                           "/notification-configuration"))
  (define h (hasheq 'Host (host)
                    'Date (seconds->gmt-8601-string 'basic (current-seconds))
                    'x-amz-glacier-version glacier-version))
  (define xs (append (if inventory? '("InventoryRetrievalCompleted") '())
                     (if archive? '("ArchiveRetrievalCompleted") '())))
  (define data (jsexpr->bytes (hasheq 'SNSTopic sns
                                      'Events xs)))
  (call/output-request "1.1"
                       m
                       u
                       (λ (out) (display data out))
                       (bytes-length data)
                       (dict-set h
                                 'Authorization
                                 (aws-v4-authorization m u h
                                                       (sha256-hex-string data)
                                                       (region) service))
                      (λ (p h)
                        (check-response p h)
                        #t)))

(define/contract (get-vault-notifications name)
  (string? . -> . jsexpr?)
  (define m "GET")
  (define u (string-append "http://" (host) "/-/vaults/" name
                           "/notification-configuration"))
  (define h (hasheq 'Host (host)
                    'Date (seconds->gmt-8601-string 'basic (current-seconds))
                    'x-amz-glacier-version glacier-version))
  (call/input-request "1.1"
                      m
                      u
                      (dict-set h
                                'Authorization
                                (aws-v4-authorization m u h
                                                      (sha256-hex-string #"")
                                                      (region) service))
                      (λ (p h)
                        (check-response p h)
                        (read-entity/jsexpr p h))))

(define/contract (delete-vault-notifications name)
  (string? . -> . void)
  (define m "DELETE")
  (define u (string-append "http://" (host) "/-/vaults/" name
                           "/notification-configuration"))
  (define h (hasheq 'Host (host)
                    'Date (seconds->gmt-8601-string 'basic (current-seconds))
                    'x-amz-glacier-version glacier-version))
  (call/input-request "1.1"
                      m
                      u
                      (dict-set h
                                'Authorization
                                (aws-v4-authorization m u h
                                                      (sha256-hex-string #"")
                                                      (region) service))
                      (λ (p h)
                        (check-response p h)
                        (void (read-entity/bytes p h)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Return the x-amz-archive-id
(define/contract (create-archive name desc data)
  (string? string? bytes?  . -> . string?)
  (define m "POST")
  (define u (string-append "http://" (host) "/-/vaults/" name "/archives"))
  (define h (hasheq 'Host (host)
                    'Date (seconds->gmt-8601-string 'basic (current-seconds))
                    'Expect "100-continue"
                    'Content-Length (bytes-length data)
                    'x-amz-glacier-version glacier-version
                    'x-amz-content-sha256 (bytes->hex-string (sha256 data))
                    'x-amz-sha256-tree-hash (bytes->hex-string (tree-hash data))
                    'x-amz-archive-description desc))
  (call/output-request "1.1"
                       m
                       u
                       (λ (out) (display data out))
                       (bytes-length data)
                       (dict-set h
                                 'Authorization
                                 (aws-v4-authorization m u h
                                                       (sha256-hex-string data)
                                                       (region) service))
                       (λ (p h)
                         (check-response p h)
                         (void (read-entity/jsexpr p h))
                         (extract-field "x-amz-archive-id" h))))

(define/contract (delete-archive vault archive-id)
  (string? string? . -> . (or/c #t exn:fail:aws?))
  (define m "DELETE")
  (define u (string-append "http://" (host) "/-/vaults/" vault
                           "/archives/" archive-id))
  (define h (hasheq 'Host (host)
                    'Date (seconds->gmt-8601-string 'basic (current-seconds))
                    'x-amz-glacier-version glacier-version))
  (call/input-request "1.1"
                      m
                      u
                      (dict-set h
                                'Authorization
                                (aws-v4-authorization m u h
                                                      (sha256-hex-string #"")
                                                      (region) service))
                      (λ (p h)
                        (check-response p h)
                        #t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (valid-part-size? n)
  (and (exact-integer? n)
       (for/or ([e (in-range 20 33 1)])      ;1MB to 4GB in powers of 2
           (equal? n (expt 2 e)))))

(define/contract (start-multipart-upload name part-size desc)
  (string? valid-part-size? string?  . -> . string?)
  (define m "POST")
  (define u (string-append "http://" (host) "/-/vaults/" name
                           "/multipart-uploads"))
  (define h (hasheq 'Host (host)
                    'Date (seconds->gmt-8601-string 'basic (current-seconds))
                    'x-amz-glacier-version glacier-version
                    'x-amz-part-size part-size
                    'x-amz-archive-description desc))
  (call/output-request "1.1"
                       m
                       u
                       (λ (out) (void))
                       0
                       (dict-set h
                                 'Authorization
                                 (aws-v4-authorization m u h
                                                       (sha256-hex-string #"")
                                                       (region) service))
                       (λ (p h)
                         (check-response p h)
                         (void (read-entity/bytes p h))
                         (extract-field "x-amz-multipart-upload-id" h))))


;; Note: (bytes-length data) must be *exactly* the part-size for every
;; part but the last, where it must be <=.
;;
;; Returns void unless it throws an exception.
(define/contract (upload-part name upload-id part-size offset data tree-hash)
  (string? string? valid-part-size? exact-nonnegative-integer? bytes? sha256? . -> . void)
  (log-aws-debug (format "upload-part ~a-~a" offset (+ offset (bytes-length data))))
  (define m "PUT")
  (define u (string-append "http://" (host) "/-/vaults/" name
                           "/multipart-uploads/" upload-id))
  (define h (hasheq 'Host (host)
                    'Date (seconds->gmt-8601-string 'basic (current-seconds))
                    'Expect "100-continue"
                    'x-amz-glacier-version glacier-version
                    'x-amz-part-size part-size
                    'Content-Length (bytes-length data)
                    'Content-Type "application/octet-stream"
                    'Content-Range (format "bytes ~a-~a/*"
                                           offset
                                           (sub1 (+ offset (bytes-length data))))
                    'x-amz-content-sha256 (bytes->hex-string (sha256 data))
                    'x-amz-sha256-tree-hash (bytes->hex-string tree-hash)))
  (let loop ((tries 12)
             (delay 1))
    (with-handlers
      ([exn?
         (λ(e)
           (if (> tries 0)
             (begin
               (log-aws-warning (format "upload-part: Retrying in ~a second~a after error: ~a"
                                        delay
                                        (if (= delay 1) "" "s")
                                        (exn-message e)))
               (sleep delay)
               (loop (sub1 tries) (* 2 delay)))
             (raise e)))])
      (call/output-request "1.1"
                           m
                           u
                           (λ (out) (display data out))
                           (bytes-length data)
                           (dict-set h
                                     'Authorization
                                     (aws-v4-authorization m u h
                                                           (sha256-hex-string data)
                                                           (region) service))
                           (λ (p h)
                             (check-response p h)
                             (void))))))

(struct threaded-upload-ctx (work-channel exn-channel thread-group))

(define/contract (make-upload-ctx)
  (-> threaded-upload-ctx?)
  (define work-channel (make-channel))
  (define exn-channel  (make-channel))
  (define (worker)
    (define mailbox-receive-evt (thread-receive-evt))
    (let loop ()
      (match (sync work-channel mailbox-receive-evt)
        [(? (curry eq? mailbox-receive-evt)) (void)]
        [(list args ...)
         (when
           (with-handlers
             ([exn? (λ(e) (channel-put exn-channel e) #f)])
             (begin
               (apply upload-part args)
               #t))
           (loop))])))
  (threaded-upload-ctx
    work-channel
    exn-channel
    (for/list ([i (in-range 0 (num-threads))])
      (thread worker))))

(define/contract (finish-upload-ctx ctx)
  (-> threaded-upload-ctx? void?)
  (for ([thread (threaded-upload-ctx-thread-group ctx)])
    (thread-send thread 'done #f))
  (for ([thread (threaded-upload-ctx-thread-group ctx)])
    (match (sync thread (threaded-upload-ctx-exn-channel ctx))
      [(? exn? e) (raise e)]
      [else (void)])))

(define/contract (upload-ctx-perform ctx . args)
  (->* (threaded-upload-ctx?) #:rest any/c void?)
  (define put-evt (channel-put-evt (threaded-upload-ctx-work-channel ctx) args))
  (match (channel-try-get (threaded-upload-ctx-exn-channel ctx))
    [(? exn? e)
     (raise e)]
    [else (void)])
  (match (sync put-evt (threaded-upload-ctx-exn-channel ctx))
    [(? (curry eq? put-evt)) (void)]
    [(? exn? e)
     (raise e)]))

(define/contract (finish-multipart-upload name upload-id total-size tree-hash)
  (string? string? exact-nonnegative-integer? sha256? . -> . string?)
  (define m "POST")
  (define u (string-append "http://" (host) "/-/vaults/" name
                           "/multipart-uploads/" upload-id))
  (define h (hasheq 'Host (host)
                    'Date (seconds->gmt-8601-string 'basic (current-seconds))
                    'x-amz-glacier-version glacier-version
                    'x-amz-archive-size total-size
                    'x-amz-sha256-tree-hash (bytes->hex-string tree-hash)))
  (call/output-request "1.1"
                       m
                       u
                       (λ (out) (void))
                       0
                       (dict-set h
                                 'Authorization
                                 (aws-v4-authorization m u h
                                                       (sha256-hex-string #"")
                                                       (region)
                                                       service))
                       (λ (p h)
                         (check-response p h)
                         (void (read-entity/bytes p h))
                         (extract-field "x-amz-archive-id" h))))

(define/contract (create-archive/multipart-upload name desc part-size data)
  (string? string? valid-part-size? bytes? . -> . string?)
  (define id (start-multipart-upload name part-size desc))
  (define len (bytes-length data))
  (define ctx (make-upload-ctx))
  (define archive-tree-hash
    (hashes->tree-hash
      (for/list ([i (in-range 0 len part-size)])
        (let* ((part (subbytes data i (min (+ i part-size) len)))
               (part-hash (tree-hash part)))
          (upload-ctx-perform ctx name id part-size i part part-hash)
          part-hash))))
  (finish-upload-ctx ctx)
  (finish-multipart-upload name id len archive-tree-hash))

(define/contract (create-archive-from-port vault port desc #:part-size [part-size 1MB])
  ((string? input-port? string?) (#:part-size valid-part-size?) . ->* . string?)
  (define id (start-multipart-upload vault part-size desc))
  (define ctx (make-upload-ctx))
  (let loop ([i 0]
             [xs '()])
    (define b (read-bytes part-size port))
    (cond
      [(eof-object? b)
       (finish-upload-ctx ctx)
       (finish-multipart-upload vault id i (hashes->tree-hash (reverse xs)))]
      [else
        (let ((part-hash (tree-hash b)))
          (upload-ctx-perform ctx vault id part-size i b part-hash)
          (loop (+ i (bytes-length b))
                (cons part-hash xs)))])))

(define/contract (create-archive-from-file vault path)
  (string? path? . -> . string?)
  (define ps (path->string path))
  (define desc (string-append ps " " (seconds->gmt-8601-string)))
  (define part-size
    (max
      1MB
      (expt 2 (sub1 (integer-length (ceiling (/ (file-size ps) 10000)))))))
  (when (> part-size (* 4 1024 1024 1024))
    (error 'create-archive-from-file "File is too large (maximum 39.06 TiB)"))
  (call-with-input-file ps
    (λ (port)
      (create-archive-from-port vault port desc #:part-size part-size))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define/contract (retrieve-inventory name desc [sns #f])
  ((string? string?) ((or/c #f string?)) . ->* . string?)
  (initiate-job name
                (append (list 'Type "inventory-retrieval"
                              'Description desc
                              'Format "JSON")
                        (if sns (list 'SNSTopic sns) '()))))

(define/contract (retrieve-archive name desc archive-id [sns #f])
  ((string? string? string?) ((or/c #f string?)) . ->* . string?)
  (initiate-job name
                (append (list 'Type "archive-retrieval"
                              'Description desc
                              'ArchiveId archive-id)
                        (if sns (list 'SNSTopic sns) '()))))

(define/contract (initiate-job name xs)
  (string? list? . -> . string?)
  (define data (jsexpr->bytes (apply hasheq xs)))
  (define m "POST")
  (define u (string-append "http://" (host) "/-/vaults/" name "/jobs"))
  (define h (hasheq 'Host (host)
                    'Date (seconds->gmt-8601-string 'basic (current-seconds))
                    'Content-Length (bytes-length data)
                    'x-amz-glacier-version glacier-version))
  (call/output-request "1.1"
                       m
                       u
                       (λ (out) (display data out))
                       (bytes-length data)
                       (dict-set h
                                 'Authorization
                                 (aws-v4-authorization m u h
                                                       (sha256-hex-string data)
                                                       (region) service))
                       (λ (p h)
                         (check-response p h)
                         (void (read-entity/bytes p h))
                         (extract-field "x-amz-job-id" h))))

;; TO-DO: Support > 1,000 by using `marker' query param
(define (list-jobs name)
  (define m "GET")
  (define u (string-append "http://" (host) "/-/vaults/" name "/jobs"))
  (define h (hasheq 'Host (host)
                    'Date (seconds->gmt-8601-string 'basic (current-seconds))
                    'x-amz-glacier-version glacier-version))
  (call/input-request "1.1"
                      m
                      u
                      (dict-set h
                                'Authorization
                                (aws-v4-authorization m u h
                                                      (sha256-hex-string #"")
                                                      (region) service))
                      (λ (p h)
                        (check-response p h)
                        (read-entity/jsexpr p h))))

(define/contract (get-job-output name job)
  (string? string? . -> . (or/c jsexpr? bytes?))
  (define m "GET")
  (define u (string-append "http://" (host) "/-/vaults/" name "/jobs/" job
                           "/output"))
  (define h (hasheq 'Host (host)
                    'Date (seconds->gmt-8601-string 'basic (current-seconds))
                    'x-amz-glacier-version glacier-version))
  (call/input-request "1.1"
                      m
                      u
                      (dict-set h
                                'Authorization
                                (aws-v4-authorization m u h
                                                      (sha256-hex-string #"")
                                                      (region) service))
                      (λ (p h)
                        (check-response p h)
                        (match (extract-field "Content-Type" h)
                         ["application/json" (read-entity/jsexpr p h)]
                         [else (read-entity/bytes p h)]))))

(define/contract (get-job-output-to-file name job path exists)
  (string? string? path? (or/c 'error 'append 'update 'replace 'truncate
                               'truncate/replace)
           . -> . boolean?)
  (define m "GET")
  (define u (string-append "http://" (host) "/-/vaults/" name "/jobs/" job
                           "/output"))
  (define h (hasheq 'Host (host)
                    'Date (seconds->gmt-8601-string 'basic (current-seconds))
                    'x-amz-glacier-version glacier-version))
  (call/input-request "1.1"
                      m
                      u
                      (dict-set h
                                'Authorization
                                (aws-v4-authorization m u h
                                                      (sha256-hex-string #"")
                                                      (region) service))
                      (λ (in h)
                        (check-response in h)
                        (with-output-to-file path
                          (λ ()
                            (read-entity/port in h (current-output-port)))
                          #:exists exists)
                        ;; Verify that x-amz-sha256-tree-hash matches.
                        ;; TO-DO: Might be better to do block by block, above.
                        (verify-file
                         path
                         (extract-field "x-amz-sha256-tree-hash" h)))))

(define (verify-file path tree-hash)
  (with-input-from-file path
    (λ ()
      (equal? (bytes->hex-string
                (hashes->tree-hash (for/list ([i (in-range 0 (file-size path) 1MB)])
                                     (sha256 (read-bytes 1MB)))))
              tree-hash))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (read-entity/jsexpr in h)
  (bytes->jsexpr (read-entity/bytes in h)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Produce a list of SHA256 hashes of each 1MB of the bytes
(define/contract (bytes->hashes b)
  (bytes? . -> . (listof sha256?))
  (define total-len (bytes-length b))
  (for/list ([i (in-range 0 total-len 1MB)])
      (sha256 (subbytes b i (min (+ i 1MB) total-len)))))

;; Given a list of hashes make a tree hash
(define/contract (hashes->tree-hash xs)
  ((listof sha256?) . -> . sha256?)
  (match xs
    [(list x) x]
    [else (hashes->tree-hash (for/list ([(a b) (in-take xs 2 (const #f))])
                               (cond [b (sha256 (bytes-append a b))]
                                     [else a])))]))

;; Given bytes? make a tree hash
(define/contract (tree-hash b)
  (bytes? . -> . sha256?)
  (hashes->tree-hash (bytes->hashes b)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Examples


;; (define/contract (show-inventory-job-output js)
;;   (jsexpr? . -> . any)
;;   (displayln "=== INVENTORY ===")
;;   (for ([(k v) (in-hash js)])
;;       (unless (equal? 'ArchiveList k)
;;         (printf "~a: ~s\n" k v)))
;;   (printf "Archives: ~a\n\n" (length (hash-ref js 'ArchiveList)))
;;   (for ([h (in-list (hash-ref js 'ArchiveList))])
;;       (displayln "--- Archive ---")
;;     (for ([(k v) (in-hash h)])
;;         (printf "~a: ~s\n" k v))
;;     (newline))
;;   (newline))

;; (for ([x (in-list (hash-ref (list-jobs "test") 'JobList '()))])
;;     (define id (hash-ref x 'JobId))
;;   (define date (hash-ref x 'CreationDate))
;;   (define type (hash-ref x 'Action))
;;   (define completed? (hash-ref x 'Completed))
;;   (define inventory? (equal? (hash-ref x 'Action "") "InventoryRetrieval"))
;;   (printf "Job ID ~s, created ~a, an ~a, ~a completed.\n"
;;           id date type (if completed? "IS" "is NOT"))
;;   (when (and completed? inventory?)
;;     (show-inventory-job-output (get-job-output "test" id))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ test
  (require rackunit
           racket/port
           racket/runtime-path
           "sns.rkt"
           "tests/data.rkt")
  (define-runtime-path aws.scrbl "aws.scrbl")
  (when (test-data-exists?)
    (define (go)
      (check-equal?
       (bytes->hex-string (tree-hash (make-bytes (* 4 1024 1024))))
       "27b557ba335a688f779a95e258a886ffa3b39b13533d6f9dcaa0497a2ed1fe18")

      (define vault (test/vault))

      (define topic-arn (create-topic (test/topic)))

      (check-true (create-vault vault))
      (check-true (for/or ([x (in-list (list-vaults))])
                    (define name (hash-ref x 'VaultName #f))
                    (and name (string=? name vault))))

      (define id #f)

      (check-not-exn
       (λ () (set! id (create-archive vault "description" #"Hello, world"))))
      (check-true (delete-archive vault id))

      (check-not-exn
       (λ ()
         (set! id (create-archive vault "description"
                                  (make-bytes (+ 3 (* 4 1MB)))))))
      (check-true (delete-archive vault id))

      (check-not-exn
       (λ ()
         (set! id (create-archive/multipart-upload vault "description" 1MB
                                                   (make-bytes (+ 3 (* 4 1MB)))))))
      (check-true (delete-archive vault id))

      (check-not-exn
       (λ ()
         (set! id (create-archive-from-file vault aws.scrbl))))
      (check-true (delete-archive vault id))

      (check-not-exn
       (λ ()
         (call-with-input-bytes
          (make-bytes (* 19 1MB))
          (λ (port)
            (set! id (create-archive-from-port vault port "test 19MB/16MB"
                                               #:part-size (* 16 1MB)))))))

      (check-true (delete-archive vault id))

      ;; Unfortunately the retrieve-XXX operations take 3-5 hours to
      ;; complete, so it's impractical for our unit test to check the SNS
      ;; topic. Furthermore, retrieve-inventory may fail during the first 24
      ;; hours after a vault is created, because Amazon Glacier hasn't
      ;; created an initial inventory yet. Gah.

      ;; (define job-id (retrieve-inventory vault "" topic-arn))
      ;; (list-jobs)

      (check-not-exn
       (λ () (set-vault-notifications vault topic-arn #t #t))))

    ;; Test a few regions. Note that when using SNS with Glacier, the
    ;; regions and endpoits must match, so be sure to set sns-endpoint
    ;; and sns-region, too.
    (for ([r '("us-east-1" "us-west-1" "eu-west-1")])
      (parameterize ([region r]
                     [sns-region r]
                     [sns-endpoint (endpoint (format "sns.~a.amazonaws.com" r) #f)])
        (go)))))
