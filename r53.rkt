#lang racket

(require net/base64
         xml
         (planet gh/http/request)
         (planet gh/http/head)
         "keys.rkt"
         "exn.rkt"
         "util.rkt"
         )

(provide r53-endpoint
         list-hosted-zones
         get-hosted-zone
         list-resource-record-sets
         )

(define r53-endpoint (make-parameter (endpoint "route53.amazonaws.com" #t)))

;; Holy cow, an AWS request signing method that's simple! The string
;; to sign is simply the value of the Date header.
(define/contract (date+authorize h)
  (dict? . -> . dict?)
  (define d (seconds->gmt-string))
  (define a (format
             "AWS3-HTTPS AWSAccessKeyId=~a,Algorithm=HmacSHA256,Signature=~a"
             (public-key)
             (sha256-encode d)))
  (dict-set* h
             'Date d
             'X-Amzn-Authorization a))

(define/contract (list-hosted-zones)
  (-> xexpr?)
  (define p "/2012-02-29/hostedzone")
  (define u (endpoint->uri (r53-endpoint) p))
  (define h (date+authorize '()))
  (call/input-request "1.1" "GET" u h
                      (lambda (in h)
                        (check-response in h)
                        (read-entity/xexpr in h))))

(define/contract (get-hosted-zone id)
  (string? . -> . xexpr?)
  (define p (string-append "/2012-02-29" id))
  (define u (endpoint->uri (r53-endpoint) p))
  (define h (date+authorize '()))
  (call/input-request "1.1" "GET" u h
                      (lambda (in h)
                        (check-response in h)
                        (read-entity/xexpr in h))))

(define record-type/c
  (or/c 'A 'AAAA 'CNAME 'MX 'NS 'PTR 'SOA 'SPF 'SRV 'TXT))

;; Return a list of (ResourceRecordSet ...) xexprs.
;;
;; Implementation note: We're using max-items to control what this
;; function returns, as well as for r53's maxitems query
;; parameter. Pay attention to the logic below.
(define/contract (list-resource-record-sets zone-id
                                            #:max-items [max-items #f]
                                            #:name [name #f]
                                            #:type [type #f]
                                            #:id [id #f])
  ((string?)
   (#:max-items (or/c #f exact-positive-integer?)
    #:name (or/c #f string?)
    #:type (or/c #f record-type/c)
    #:id (or/c #f string?))
   . ->* . (listof xexpr?))
  (let loop ([max-items max-items]
             [name name]
             [type type]
             [id id])
    (define d (apply dict-set* (cons (hash)
                                     (true-value-pairs 'maxitems max-items
                                                       'name name
                                                       'type type
                                                       'identifier id))))
    (define qp (dict->form-urlencoded d))
    (define p (string-append "/2012-02-29" zone-id "/rrset?" qp))
    (define u (endpoint->uri (r53-endpoint) p))
    (define h (date+authorize '()))
    (define x (call/input-request "1.1" "GET" u h
                                  (lambda (in h)
                                    (check-response in h)
                                    (read-entity/xexpr in h))))
    (define rs (or (tags x 'ResourceRecordSet) '()))
    (define len (length rs))
    (append rs
            (cond [(and (< len max-items)
                        (string-ci=? "true" (first-tag-value x 'IsTruncated)))
                   (loop (min max-items len)
                         (first-tag-value x 'NextRecordName)
                         (first-tag-value x 'NextRecordType)
                         (first-tag-value x 'NextRecordId))]
                  [else '()]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ test
  (require "run-suite.rkt")
  (def/run-test-suite
    (test-case
     "Route53"
     (ensure-have-keys)
     (define zs (list-hosted-zones))
     (define zid (first-tag-value zs 'Id)) ;just grab first one
     (define name (first-tag-value zs 'Name)) ;just grab first
     (check-true (match (get-hosted-zone zid)
                   [`(GetHostedZoneResponse
                      ((xmlns "https://route53.amazonaws.com/doc/2012-02-29/"))
                      (HostedZone ()
                                  (Id () ,zid)
                                  ,_ ...)
                      ,_ ...)
                    #t]
                   [_ #f]))

     (check-true (match (list-resource-record-sets zid
                                                   #:name name
                                                   #:max-items 1)
                   [`((ResourceRecordSet ()
                                         (Name () ,name)
                                         ,_ ...))
                    #t]
                   [_ #f]))

     (check-equal? (list-resource-record-sets zid
                                              ;; Unlikely domain:
                                              #:name "zzzzzzzzzzzz.com."
                                              #:max-items 100)
                   '())
    )))
