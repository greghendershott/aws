#lang racket/base

(require http/request
         racket/contract/base
         racket/contract/region
         racket/dict
         racket/match
         xml
         "exn.rkt"
         "sigv4.rkt"
         "util.rkt")

(provide r53-endpoint
         create-hosted-zone
         delete-hosted-zone
         list-hosted-zones
         domain-name->zone-id
         get-hosted-zone
         list-resource-record-sets
         change-resource-record-sets)

(define r53-endpoint (make-parameter (endpoint "route53.amazonaws.com" #t)))

(define/contract (date+authorize method uri heads body)
  (-> string? string? dict? bytes? dict?)
  (let ([heads (dict-set* heads
                          'Host (endpoint-host (r53-endpoint))
                          'Date (seconds->gmt-8601-string 'basic
                                                          (current-seconds)))])
    (dict-set* heads
               "Authorization"
               (aws-v4-authorization
                method
                uri
                heads
                (sha256-hex-string body)
                "us-east-1"
                "route53"))))


;;; hosted zones

(define/contract (create-hosted-zone name unique [comment ""])
  (->* (string? string?) (string?) xexpr?)
  (define p "/2012-02-29/hostedzone")
  (define u (endpoint->uri (r53-endpoint) p))
  (define bstr (string->bytes/utf-8
                (xexpr->string
                 `(CreateHostedZoneRequest
                   ([xmlns "https://route53.amazonaws.com/doc/2012-02-29/"])
                   (Name () ,name)
                   (CallerReference () ,unique)
                   (HostedZoneConfig () (Comment () ,comment))))))
  (define h (date+authorize "POST"
                            u
                            (hasheq 'Content-Type "application/xml")
                            bstr))
  (call/output-request "1.1" "POST" u bstr (bytes-length bstr) h
                       (λ (in h)
                         (check-response in h)
                         (read-entity/xexpr in h))))

(define/contract (delete-hosted-zone id)
  (-> string? xexpr?)
  (define p (string-append "/2012-02-29" id))
  (define u (endpoint->uri (r53-endpoint) p))
  (define h (date+authorize "DELETE" u '() #""))
  (call/input-request "1.1" "DELETE" u h
                      (λ (in h)
                        (check-response in h)
                        (read-entity/xexpr in h))))

(define/contract (list-hosted-zones)
  (-> xexpr?)
  (define p "/2012-02-29/hostedzone")
  (define u (endpoint->uri (r53-endpoint) p))
  (define h (date+authorize "GET" u '() #""))
  (call/input-request "1.1" "GET" u h
                      (λ (in h)
                        (check-response in h)
                        (read-entity/xexpr in h))))

(define/contract (get-hosted-zone id)
  (-> string? xexpr?)
  (define p (string-append "/2012-02-29" id))
  (define u (endpoint->uri (r53-endpoint) p))
  (define h (date+authorize "GET" u '() #""))
  (call/input-request "1.1" "GET" u h
                      (λ (in h)
                        (check-response in h)
                        (read-entity/xexpr in h))))

(define/contract (domain-name->zone-id name)
  (-> string? (or/c #f string?))
  (let ([name (match name           ;helpfully (?) append . if missing
                [(pregexp "\\.$") name]
                [_ (string-append name ".")])])
    (for/or ([zone (in-list (se-path*/elements '(HostedZone) (list-hosted-zones)))])
      (match (se-path* '(Name) zone)
        [(== name) (se-path* '(Id) zone)]
        [_ #f]))))


;;; record sets

(define record-type/c
  (or/c 'A 'AAAA 'CNAME 'MX 'NS 'PTR 'SOA 'SPF 'SRV 'TXT))

;; Return a list of (ResourceRecordSet ...) xexprs.
;;
;; Implementation note: We're using max-items to control what this
;; function returns, as well as for r53's maxitems query
;; parameter. Pay attention to the logic below.
(define/contract (list-resource-record-sets zone-id
                                            #:max-items [max-items 100]
                                            #:name [name #f]
                                            #:type [type #f]
                                            #:id [id #f])
  (->* (string?)
       (#:max-items (or/c #f exact-positive-integer?)
        #:name (or/c #f string?)
        #:type (or/c #f record-type/c)
        #:id (or/c #f string?))
       (listof xexpr?))
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
    (define h (date+authorize "GET" u '() #""))
    (define x (call/input-request "1.1" "GET" u h
                                  (λ (in h)
                                    (check-response in h)
                                    (read-entity/xexpr in h))))
    (define rs (se-path*/elements '(ResourceRecordSet) x))
    (define len (length rs))
    (append rs
            (cond [(and (< len max-items)
                        (string-ci=? "true" (se-path* '(IsTruncated) x)))
                   (loop (min max-items len)
                         (se-path* '(NextRecordName) x)
                         (se-path* '(NextRecordType) x)
                         (se-path* '(NextRecordId) x))]
                  [else '()]))))

;; It's up to the caller to create an xexpr according to
;; http://docs.amazonwebservices.com/Route53/latest/APIReference/API_ChangeResourceRecordSets.html.
;; Poor cost:benefit to wrap the XML permutations in structs.
(define/contract (change-resource-record-sets zone-id changes)
  (-> string? xexpr? xexpr?)
  (define p (string-append "/2012-02-29" zone-id "/rrset"))
  (define u (endpoint->uri (r53-endpoint) p))
  (define bstr (string->bytes/utf-8 (xexpr->string changes)))
  (define h (date+authorize "GET"
                            u
                            (hasheq 'Content-Type "application/xml")
                            bstr))
  (call/output-request "1.1" "POST" u bstr (bytes-length bstr) h
                       (λ (in h)
                         (check-response in h)
                         (read-entity/xexpr in h))))

;; Example:
;; (change-resource-record-sets
;;  "/hostedzone/Z3K3IRK2M12WGD"
;;  `(ChangeResourceRecordSetsRequest
;;    ([xmlns "https://route53.amazonaws.com/doc/2012-02-29/"])
;;    (ChangeBatch
;;     (Comment "optional comment about the changes in this change batch request")
;;     (Changes (Change
;;               (Action "CREATE")
;;               (ResourceRecordSet (Name "foo2.com")
;;                                  (Type "A")
;;                                  (TTL "300")
;;                                  (ResourceRecords
;;                                   (ResourceRecord
;;                                    (Value "1.2.3.4")))))))))

(module+ test
  (require rackunit
           "keys.rkt"
           "tests/data.rkt")
  (when (test-data-exists?)
   (test-case
     "Route53 create/delete hosted zone"
     (ensure-have-keys)
     (define x (create-hosted-zone (test/domain.com)
                                   (format "unique-~a" (current-seconds))
                                   "some comment"))
     (check-true (xexpr? x))
     (define zid (se-path* '(Id) x))
     (check-true (string? zid))
     (delete-hosted-zone zid)
     (void))
    (test-case
     "Route53 read-only record sets"
     (ensure-have-keys)
     (define zs (list-hosted-zones))
     (define zid (se-path* '(Id) zs)) ;just grab first one
     (define name (se-path* '(Name) zs)) ;just grab first
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
                   '()))))
