#lang racket/base

(require (for-syntax racket/base)
         racket/contract/base
         racket/contract/region
         racket/list
         racket/match
         racket/set
         xml/xexpr
         "keys.rkt"
         "post.rkt"
         "util.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Wrappers for the core `sdb' function.
;;

;; Set this parameter to #t to make the Item.Replace true for all calls.
;; Else if at default #f value, Replace will be specified only if you do
;; it for each attribute, using (key val 'replace) instead of (key val).
(define always-replace? (make-parameter #f))
(provide always-replace?)

(define attribs/c (listof (or/c (list/c symbol? string?)
                                (list/c symbol? string? 'replace))))
(provide attribs/c)

(define/contract/provide (create-domain domain-name)
  (string? . -> . any)
  (void (sdb `((Action "CreateDomain")
               (DomainName ,domain-name)))))

(define/contract/provide (delete-domain domain-name)
  (string? . -> . any)
  (void (sdb `((Action "DeleteDomain")
               (DomainName ,domain-name)))))

(define/contract/provide (list-domains [max 100])
  (()
   ((and/c integer? (between/c 1 100)))
   . ->* . attribs/c)
  (sdb `((Action "ListDomains"))
       (lambda (x)
         (map (lambda (i) (list (first i) (third i)))
              (tags x 'DomainName)))))

(define/contract/provide (domain-metadata domain-name)
  (string? . -> . attribs/c)
  (sdb `((Action "DomainMetadata")
         (DomainName ,domain-name))
       (lambda (x)
         ;; Response includes:
         ;;   ((DomainMetadataResult () (k0 () v0) (k1 () v1) ... ))
         (match (cddar (tags x 'DomainMetadataResult))
           [(list (list k a v) ...)
            (apply map list (list k v))]))))

(define/contract/provide (put-attributes domain-name item-name attributes)
  (string? string? attribs/c . -> . any)
  (void (sdb `((Action "PutAttributes")
               (DomainName ,domain-name)
               (ItemName ,item-name)
               ,@(attributes->query-params attributes)))))

(define/contract/provide (get-attributes domain-name item-name)
  (string? string? . -> . attribs/c)
  (sdb `((Action "GetAttributes")
         (DomainName ,domain-name)
         (ItemName ,item-name))))

(define/contract/provide (delete-attributes domain-name item-name attributes)
  (string? string? attribs/c . -> . any)
  (void (sdb `((Action "DeleteAttributes")
               (DomainName ,domain-name)
               (ItemName ,item-name)
               ,@(attributes->query-params attributes)))))

(define/contract/provide (delete-item domain-name item-name)
  (string? string? . -> . any)
  ;; SDB doesn't provide a delete-item per se. Instead, for a given
  ;; item-name, get all of its attributes, then delete all of them.
  (delete-attributes domain-name
                     item-name
                     (get-attributes domain-name
                                     item-name)))
(define/contract/provide (select expr)
  (string? . -> . (listof attribs/c))
  (sdb `((Action "Select")
         (SelectExpression ,expr))
       (lambda (x)
         (for/list ([i (in-list (tags x 'Item))])
             (cons (list 'ItemName (third (third i)))
                   (map attribute-xexpr->attrib-pair
                        (tags i 'Attribute)))))))

(define/contract/provide (batch-put-attributes domain-name xs)
  (string? (listof (cons/c string? attribs/c)) . -> . any)
  (void (sdb `((Action "BatchPutAttributes")
               (DomainName ,domain-name)
               ,@(batch-attributes->query-params xs)))))

(define/contract/provide (batch-delete-attributes domain-name xs)
  (string? (listof (cons/c string? attribs/c)) . -> . any)
  (void (sdb `((Action "BatchDeleteAttributes")
               (DomainName ,domain-name)
               ,@(batch-attributes->query-params xs)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Convert a list of SDB attributes into an list of the query
;; parameters. There is at least a pair of query parameters for each
;; attribute: Attribute.N.Name and Attribute.N.Value. And optionally
;; an Attribute.N.Replace parameter.
(define/contract (attributes->query-params al)
  (attribs/c . -> . any #| attribs/c |#)
  (for/fold ([xs '()])
      ([s (in-list al)]
       [n (in-naturals 1)])
    (define-values (name value replace?)
      (match s
        [(list name value) (values name value #f)]
        [(list name value 'replace) (values name value #t)]
        [else (error 'attributes->query-params s)]))
    (append xs
            `((,(string->symbol (format "Attribute.~a.Name" n))
               ,(symbol->string name))
              (,(string->symbol (format "Attribute.~a.Value" n))
               ,value))
            (if (or replace? (always-replace?))
                `((,(string->symbol (format "Attribute.~a.Replace" n))
                   "true"))
                '()))))

(define/contract (batch-attributes->query-params bal)
  ((listof (cons/c string? attribs/c)) . -> . attribs/c)
  (for/fold ([xs '()])
      ([item (in-list bal)]
       [n-item (in-naturals 1)])
    (append xs
            (list (list (string->symbol (format "Item.~a.ItemName" n-item))
                        (car item)))
            (for/fold ([xs '()])
                ([attr (in-list (cdr item))]
                 [n-attr (in-naturals 1)])
              (define-values (name value replace?)
                (match attr
                  [(list name value) (values name value #f)]
                  [(list name value 'replace) (values name value #t)]))
              (append
               xs
               (list (list (string->symbol (format "Item.~a.Attribute.~a.Name"
                                                   n-item n-attr))
                           (symbol->string name))
                     (list (string->symbol (format "Item.~a.Attribute.~a.Value"
                                                   n-item n-attr))
                           value))
               (if (or replace? (always-replace?))
                   (list (string->symbol (format "Item.~a.Attribute.~a.Replace"
                                                 n-item n-attr))
                           "true")
                   '()))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Default result-proc for `sdb'. Processes XML <Attribute> elements
;; into an alist.
(define (xexpr->alist x)
  (map attribute-xexpr->attrib-pair
       (tags x 'Attribute)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define sdb-endpoint (make-parameter (endpoint "sdb.amazonaws.com" #t)))
(provide sdb-endpoint)

;; `sdb' serves as the core function for most of the SDB functions.
;; We provide contracted wrappers that call this.
;;
;; params: An list of SDB parameters.
;;
;; result-proc: This is called with the response XML so that it can be
;; parsed into a list of items. Note that a single call to `sdb' may
;; lead to multiple calls to result-proc (because SDB may require us
;; to make multiple calls when there is lots of data, and result-proc
;; will be called once for each NextToken batch of results). Also note
;; that result-proc is *not* called in error conditions; it is only
;; called for 200 OK responses.
(define/contract (sdb params [result-proc xexpr->alist])
  ((attribs/c) ((xexpr? . -> . list?)) . ->* . list?)
  (ensure-have-keys)
  (define common-params
    `((AWSAccessKeyId ,(public-key))
      (SignatureMethod "HmacSHA256")
      (SignatureVersion "2")
      (Timestamp ,(timestamp))
      (Version "2009-04-15")))
  (define all-params (sort (append params common-params)
                           (lambda (a b)
                             (string<? (symbol->string (car a))
                                       (symbol->string (car b))))))
  (define str-to-sign
    (string-append "POST" "\n"
                   (endpoint->host:port (sdb-endpoint)) "\n"
                   "/" "\n"
                   (dict->form-urlencoded all-params)))
  (define signature (sha256-encode str-to-sign))
  (define signed-params (append all-params `((Signature ,signature))))
  (define header
    (hash 'Content-Type "application/x-www-form-urlencoded; charset=utf-8"))
  (define uri (endpoint->uri (sdb-endpoint) "/?"))
  (define x (post-with-retry uri signed-params header))
  (append (result-proc x)
          ;; If SDB returned a NextToken element in the response XML, we
          ;; need to call again to get more values.
          (match (tags x 'NextToken)
            [(list `(NextToken () ,token))
             (sdb (set-next-token params token)
                  result-proc)]
             [else '()])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; put-attributes and get-attributes are a low-level interface that
;; wraps SDB fairly thinly.  When you want exact control, use them.
;;
;; put-attributes-hash and get-attributes-hash provide a set-oriented
;; interface. For a multi-valued attribute, you get and set all its
;; values together as one set.  The attribute's values are represented
;; as (set/c string?). A collection of attributes is (hash/c symbol?
;; (set/c string?). When you get a multi-valued attribute, all of its
;; values are grouped and presented as a (set/c string?). When you put
;; the attribute set, all of its existing values in SDB are replaced
;; by the new set of values you supply. (At a lower level, this means
;; the first attribute is put to SDB using parameter Replace=true --
;; to clear any/all existing values. The other values for the
;; attribute are put with Replace=false -- to preserve all of the
;; multiple new values we are setting.)

(define attribs-hash/c (hash/c symbol? set? #|(set/c string?)|#))

(define/contract (attribs-hash/c->attribs/c attribs-hash/c)
  (attribs-hash/c . -> . attribs/c)
  (for/fold ([xs '()])
      ([(k v) (in-hash attribs-hash/c)])
    (append xs
            (for/list ([v (in-set v)]
                       [n (in-naturals 0)])
                (if (zero? n)
                    (list k v 'replace)
                    (list k v))))))
                    
(define/contract (attribs/c->attribs-hash/c xs)
  (attribs/c . -> . attribs-hash/c)
  (let ([h (make-hash)])
    (for ([x (in-list xs)])
        (match-let ([(list k v) x])
          (let ([s (hash-ref h k #f)])
            (if s
                (hash-set! h k (set-add s v))
                (hash-set! h k (set v))))))
    h))

(define/contract/provide (put-attributes-hash domain-name item-name attribs)
  (string? string? attribs-hash/c . -> . any)
  (parameterize ([always-replace? #f])
    (put-attributes domain-name item-name (attribs-hash/c->attribs/c attribs))))

(define/contract/provide (get-attributes-hash domain-name item-name)
  (string? string? . -> . attribs-hash/c)
  (attribs/c->attribs-hash/c (get-attributes domain-name item-name)))

(struct item (name attribs) #:transparent)
(provide (struct-out item))

(define/contract/provide (select-hash expr)
  (string? . -> . (listof item?))
  (sdb `((Action "Select")
         (SelectExpression ,expr))
       (lambda (x)
         (for/list ([i (in-list (tags x 'Item))])
             (item (third (third i)) ; ItemName value
                   (attribs/c->attribs-hash/c
                    (map attribute-xexpr->attrib-pair
                         (tags i 'Attribute))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SDB stores all values as strings. How a number is represented as a
;; string, matters for sorts and compares. SDB docs recommend you pad
;; them with leading zeroes and apply an offset to negative numbers so
;; they are stored as a nonnegative number.
;;
;; This function makes a pair of procs, you can use to convert in both
;; ways.  You may want to make such a converter for each "data type",
;; such as small unsigned ints, small signed ints, etc.
(define/provide (int<->str [width 5] [offset 0] [pad-char #\0])
  (values
   ;; int->str
   (lambda (n)
     (define s (number->string (+ n offset)))
     (define pad (make-string (- width (string-length s)) pad-char))
     (string-append pad s))
   ;; str->int
   (lambda (s)
     (define n (string->number s))
     (- n offset))))

;; Provide some conversions for common types:
;; int->str/uint8  str->int/uint8
;; int->str/uint16 str->int/unit16
;; ...
(define-syntax (d/p-cnv stx)
  (syntax-case stx ()
    [(_ name width ofs)
     (let ([make-id
            (lambda (template . ids)
              (let ([str (apply format template (map syntax->datum ids))])
                (datum->syntax stx (string->symbol str))))])
       (with-syntax ([int->str (make-id "int->str/~a" #'name)]
                     [str->int (make-id "str->int/~a" #'name)])
       #`(begin
           (define-values (int->str str->int) (int<->str width ofs))
           (provide int->str str->int))))]))

(d/p-cnv u8  3 0)
(d/p-cnv s8  3 (expt 2 (sub1 8)))
(d/p-cnv u16 5 0)
(d/p-cnv s16 5 (expt 2 (sub1 16)))
(d/p-cnv u32 10 0)
(d/p-cnv s32 10 (expt 2 (sub1 32)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test

(module+ test
  (require rackunit
           "exn.rkt"
           "tests/data.rkt")

  (test-case
   "int<->str"
   ;; u8
   (check-equal? (str->int/u8 (int->str/u8 0)) 0)
   (check-equal? (str->int/u8 (int->str/u8 (expt 2 8))) (expt 2 8))
   ;; s8
   (check-equal? (str->int/s8 (int->str/s8 (- (expt 2 7)))) (- (expt 2 7)))
   (check-equal? (str->int/s8 (int->str/s8 (+ (expt 2 7)))) (+ (expt 2 7)))
   ;; u16
   (check-equal? (str->int/u16 (int->str/u16 0)) 0)
   (check-equal? (str->int/u16 (int->str/u16 (expt 2 16))) (expt 2 16))
   ;; s16
   (check-equal? (str->int/s16 (int->str/s16 (- (expt 2 15)))) (- (expt 2 15)))
   (check-equal? (str->int/s16 (int->str/s16 (+ (expt 2 15)))) (+ (expt 2 15)))
   ;; u32
   (check-equal? (str->int/u32 (int->str/u32 0)) 0)
   (check-equal? (str->int/u32 (int->str/u32 (expt 2 32))) (expt 2 32))
   ;; s32
   (check-equal? (str->int/s32 (int->str/s32 (- (expt 2 31)))) (- (expt 2 31)))
   (check-equal? (str->int/s32 (int->str/s32 (+ (expt 2 31)))) (+ (expt 2 31))))

  (when (test-data-exists?)
    ;; Why are there `sleep's between some tests below? Because SDB has
    ;; *eventual* consistency: There may be some delay before we will get
    ;; what we just put (or *not* get what we just deleted).

    (define (member? v xs) (not (not (member v xs))))

    (define (attrib<? a b)
      (string<? (symbol->string (car a))
                (symbol->string (car b))))

    (define (attribs-hash=? a b)
      (for/and ([(k v) (in-hash a)])
        (let ([v/b (hash-ref b k)])
          (and v/b
               (equal? v v/b)))))

    (define (item=? a b)
      (and (equal? (item-name a) (item-name b))
           (attribs-hash=? (item-attribs a) (item-attribs b))))

    (test-case
     "domains"
     (read-keys)
     (check-not-exn (lambda () (delete-domain (test/domain)))) ;in case failed prev
     (check-not-exn (lambda () (create-domain (test/domain))))
     (sleep 1)
     (check-true
      (member? `(DomainName ,(test/domain)) (list-domains)))
     (check-true
      (member? `(ItemCount "0") (domain-metadata (test/domain))))
     (check-not-exn (lambda () (delete-domain (test/domain)))))

    (test-case
     "attributes"
     (check-not-exn (lambda () (delete-domain (test/domain)))) ;in case failed prev
     (check-not-exn (lambda () (create-domain (test/domain))))
     (define attribs '((BPM "130")
                       (Genre "Disco")))
     (check-not-exn (lambda () (put-attributes (test/domain) "item" attribs)))
     (sleep 1)
     (check-equal? (get-attributes (test/domain) "item")
                   attribs)
     (check-equal? (select (string-append "select Genre from " (test/domain)))
                   `(((ItemName "item") (Genre "Disco"))))
     (check-not-exn (lambda () (delete-attributes (test/domain) "item" attribs)))
     (sleep 1)
     (check-equal? (get-attributes (test/domain) "item")
                   '())

     (define cnt 25)
     (for ([n (in-range cnt)])
       (check-not-exn
        (lambda ()
          (put-attributes (test/domain)
                          (format "Item~a" n)
                          `((n ,(format "~a" n)))))))
     (for ([n (in-range cnt)])
       (check-equal? (get-attributes (test/domain) (format "Item~a" n))
                     `((n ,(format "~a" n)))))
     (check-equal? (select (string-append "SELECT Count(*) FROM " (test/domain)))
                   `(((ItemName "Domain") (Count ,(format "~a" cnt)))))
     (for ([n (in-range cnt)])
       (check-not-exn
        (lambda ()
          (delete-attributes (test/domain)
                             (format "Item~a" n)
                             `((n ,(format "~a" n)))))))
     (for ([n (in-range cnt)])
       (check-equal? (get-attributes (test/domain) (format "Item~a" n))
                     '()))

     ;; BatchXxxAttributes
     (define (batch-attribs n)
       (for/list ([i (in-range 50)])
         (list (string->symbol (format "key/~a/~a" n i))
               (format "val/~a/~a" n i))))
     (define batch-item-count 25)
     (define (batch-items)
       (for/list ([n (in-range batch-item-count)])
         (cons (format "item~a" n)
               (batch-attribs n))))
     (check-not-exn (lambda () (batch-put-attributes (test/domain) (batch-items))))
     (sleep 10)
     (for ([n (in-range batch-item-count)])
       (check-equal? (sort ;order from SDB is undetermined
                      (get-attributes (test/domain) (format "item~a" n))
                      attrib<?)
                     (sort (batch-attribs n) attrib<?)))
     (check-not-exn
      (lambda ()
        (batch-delete-attributes (test/domain) (batch-items))))
     (sleep 10)
     (for ([n (in-range batch-item-count)])
       (check-equal? (get-attributes (test/domain) (format "item~a" n)) '()))

     ;; hash style
     (define attribs/hash (hash 'bpm   (set "100")
                                'genre (set "Rock" "Metal")))
     (check-not-exn
      (lambda () (put-attributes-hash (test/domain) "itemHash" attribs/hash)))
     (sleep 1)
     (check-true
      (attribs-hash=? (get-attributes-hash (test/domain) "itemHash")
                      attribs/hash))
     (check-true
      (item=? (car (select-hash
                    (format "select * from ~a where ItemName() = 'itemHash'"
                            (test/domain))))
              (item "itemHash" attribs/hash)))

     (check-not-exn (lambda () (delete-domain (test/domain)))))

    (test-case
     "400 errors"
     ;; Check it raises an exn:fail:aws when domain doesn't exist
     (define bad-domain-msg "The specified domain does not exist.")
     (define-syntax-rule (400-error? expr)
       (check-true
        (with-handlers
            ([exn:fail:aws?
              (lambda (exn)
                (match exn
                  [(exn:fail:aws _
                                 _
                                 400
                                 "Bad Request"
                                 "NoSuchDomain"
                                 "The specified domain does not exist.")
                   #t]
                  [else #f]))])
          ;; We expect expr to raise an exception. Return #f if it doesn't
          (begin expr #f))))
     (400-error? (select "SELECT Count(*) FROM barf"))
     (400-error? (select "SELECT Count(*) FROM barf"))
     (400-error? (put-attributes "barf" "item" '((key "val"))))
     (400-error? (get-attributes "barf" "item"))
     (400-error? (delete-attributes "barf" "item" '((key "val")))))))
