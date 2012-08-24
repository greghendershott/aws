#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; S3
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|

(require (planet gh/aws/keys)
         (planet gh/aws/s3))

(define (member? x xs)
  (not (not (member x xs))))

;; Make a random name for the bucket. Remember bucket names are a
;; global space shared by all AWS accounts. In a real-world app, if
;; you have a domain name, you probably want to include that as part
;; of your name.
(define bucket-name
  (for/fold ([s "test.bucket."])
            ([x (in-range 32)])
    (string-append s
                   (number->string (truncate (random 15)) 16))))

(ensure-have-keys)

(create-bucket bucket-name)
(member? bucket-name (list-buckets))

(define pathname "path/to/file")
(define b+p (string-append bucket-name "/" pathname))

(define data #"Hello, world.")
(put/bytes b+p data "text/plain")
(get/bytes b+p)
(get/bytes b+p '() 0 5) ; HTTP Range request
(head b+p)

(ls (string-append bucket-name "/"))
(ls (string-append bucket-name "/" pathname))
(ls (string-append bucket-name "/" (substring pathname 0 2)))

(define p (build-path 'same
                      "tests"
                      "s3-test-file-to-get-and-put.txt"))
(put/file b+p p #:mime-type "text/plain")
(get/file b+p p #:exists 'replace)
(head b+p)
(member? pathname (ls b+p))

(define b+p/copy (string-append b+p "-copy"))
(copy b+p b+p/copy)
(ls (string-append bucket-name "/"))
(head b+p/copy)
(delete b+p/copy)

(delete b+p)
(delete-bucket bucket-name)

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; SDB
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|

(require (planet gh/aws/keys)
         (planet gh/aws/sdb))

(define test-domain "TestDomain")

(ensure-have-keys)

(delete-domain test-domain)
(create-domain test-domain)
(sleep 1)
(member? `(DomainName ,test-domain) (list-domains))
(domain-metadata test-domain)

(define attribs '((BPM "130")
                  (Genre "Disco")))
(put-attributes test-domain "item" attribs)
(sleep 1)
(get-attributes test-domain "item")
(select (string-append "select Genre from " test-domain))
(delete-attributes test-domain "item" attribs)
(sleep 1)
(get-attributes test-domain "item")

(define cnt 5)
(for ([n (in-range cnt)])
    (put-attributes test-domain
                    (format "Item~a" n)
                    `((n ,(format "~a" n)))))
(for ([n (in-range cnt)])
    (displayln (get-attributes test-domain (format "Item~a" n))))

(select (string-append "SELECT Count(*) FROM " test-domain))
(select (string-append "SELECT * FROM " test-domain))

(for ([n (in-range cnt)])
    (delete-attributes test-domain
                       (format "Item~a" n)
                       `((n ,(format "~a" n)))))
(for ([n (in-range cnt)])
    (displayln (get-attributes test-domain (format "Item~a" n))))

;; BatchXxxAttributes
(define (batch-attribs n)
  (for/list ([i (in-range 6)])
      (list (string->symbol (format "key/~a/~a" n i))
            (format "val/~a/~a" n i))))
(define batch-item-count 5)
(define (batch-items)
  (for/list ([n (in-range batch-item-count)])
      (cons (format "item~a" n)
            (batch-attribs n))))
(batch-put-attributes test-domain (batch-items))
(sleep 3)
(for ([n (in-range batch-item-count)])
    (printf "item~a:\n" n)
    (displayln (get-attributes test-domain (format "item~a" n))))
(batch-delete-attributes test-domain (batch-items))
(sleep 3)
(for ([n (in-range batch-item-count)])
    (displayln (get-attributes test-domain (format "item~a" n))))

;; hash style
(define attribs/hash (hash 'bpm   (set "100")
                           'genre (set "Rock" "Metal")))
(put-attributes-hash test-domain "itemHash" attribs/hash)
(sleep 1)
(get-attributes-hash test-domain "itemHash")
(select-hash (format "SELECT * FROM ~a WHERE ItemName() = 'itemHash'"
                     test-domain))

(delete-domain test-domain)

|#
