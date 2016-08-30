#lang racket/base

(require (for-syntax racket/base)
         net/uri-codec
         racket/contract/base
         racket/contract/region
         racket/dict
         racket/list
         racket/match
         racket/port
         racket/string
         xml/xexpr
         "take.rkt"
         "xml-path.rkt")

(provide (all-from-out "xml-path.rkt"))

(module+ test
  (require rackunit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Convenience macros to define, contract, and/or provide (i.e. other
;; permutations of those, like the standard define/contract).

(define-syntax define/contract/provide
  (syntax-rules ()
    [(_ (id . args) contract body ...)
     (begin
       (define/contract (id . args) contract body ...)
       (provide id))]
    [(_ id contract expr)
     (begin
       (define/contract id contract expr)
       (provide id))]))

(define-syntax define/provide
  (syntax-rules ()
    [(_ (id . args) body ...)
     (begin
       (define (id . args) body ...)
       (provide id))]
    [(_ id expr)
     (begin
       (define id expr)
       (provide id))] ))

(provide define/contract/provide
         define/provide)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; NOTE: This predates racket/format and could probably be obsoleted.
;;
;; (any/c ... -> string?)  Given any number of Racket identifiers or
;; literals, convert them to a string. Non-literals are printed as
;; "expression = value".  For use with e.g. log-debug which takes a
;; single string? not format or printf style, plus, we want to show
;; the expression = value thing.
(define-syntax tr
  (syntax-rules ()
    [(_ e)
     (if (or (string? (syntax-e #'e))
             (number? (syntax-e #'e)))
         (format "~a" e)
         (format "~s=~a"
                 (syntax->datum #'e)
                 e))]
    [(_ e0 e1 ...)
     (string-append (tr e0)
                    " "
                    (tr e1 ...))]))

(provide tr)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; attribs <-> alist.
;; Former is (list/c symbol? string?), latter is (cons/c symbol? string?).
;; Although alists are standard Scheme/Racket fare, with xexprs we want
;; attribs, so will need to convert between sometimes.
(define/provide (attribs->alist xs)
  (define (list->cons xs)
    (match xs
      [(list k v) (cons k v)]
      [else (error 'attribs->alist "expected list of 2 items, got ~a" xs)]))
  (map list->cons xs))

(module+ test
  (check-exn exn:fail? (lambda () (attribs->alist '([a 1 more]))))
  (check-equal? (attribs->alist '([a 1] [b 2])) '([a . 1][b . 2])))

(define/provide (alist->attribs xs)
  (define (cons->list pr)
    (match pr
      [(cons k v) (list k v)]
      [else (error 'alist->attribs "expected cons, got ~a" pr)]))
  (map cons->list xs))

(module+ test
  (check-equal? (alist->attribs '([a . 1][b . 2])) '([a 1] [b 2])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Percent-encoding. Racket net/uri-codec does RFC 2396 but we want
;; RFC 3986.

(define (percent-encode c)
  (string-upcase (format "%~x" (char->integer c))))

;; The extra chars that uri-encode misses but 3986 wants
(define chars-to-encode (list #\! #\'#\(#\) #\*))

(define/provide (uri-encode/rfc-3986 s)
  (call-with-output-string
    (lambda (out)
      (for ([c (in-string (uri-encode s))])
        (if (member c chars-to-encode)
            (write-string (percent-encode c) out)
            (write-char c out))))))

;; Like Racket alist->form-urlencoded, but:
;; 1. Works on any dict? (not just an association list of cons pairs).
;; 2. Uses RFC 3986 encoding.
;; 3. The values can by any/c not just string?. Their ~a format is used.
(define/contract/provide (dict->form-urlencoded xs)
  (dict? . -> . string?)
  (define (value x)
    (match x
      [(list v) (value v)]              ;permit [k v] not just [k . v]
      [v        (format "~a" v)]))
  (string-join (for/list ([(k v) (in-dict xs)])
                   (format "~a=~a"
                           k
                           (uri-encode/rfc-3986 (value v))))
               "&"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct endpoint (host ssl?) #:transparent)
(provide (struct-out endpoint))

(define/contract/provide (endpoint->host:port x)
  (endpoint? . -> . string?)
  (match-define (endpoint host ssl?) x)
  (string-append host (if ssl? ":443" "")))

(define/contract/provide (endpoint->uri x path)
  (endpoint? string? . -> . string?)
  (match-define (endpoint host ssl?) x)
  (string-append (if ssl? "https" "http")
                 "://"
                 host
                 (if ssl? ":443" "")
                 path))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Much like `hash' lets you supply the pairs as a flat list, `alist'
;; lets you do so for an association list. Saves some tedious typing
;; of parens and dots.
(define/provide (alist . xs)
  (for/list ([(k v) (in-take xs 2)])
    (cons k v)))

(module+ test
  (define xs '([a . 1][b . 2]))
  (check-equal? (alist 'a 1 'b 2) xs)
  (check-equal? (apply alist '(a 1 b 2)) xs))

;; Given a list of couples k0 v0 k1 v1 ... kN vN, return the k v couples
;; where v is not #f.
(define/provide (true-value-pairs . xs)
  (flatten (for/list ([(k v) (in-take xs 2)]
                      #:when v)
             (list k v))))

(module+ test
  (check-equal? (true-value-pairs 'a 1 'b #f 'c 2)
                (list 'a 1 'c 2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-logger aws)
(provide log-aws-fatal
         log-aws-error
         log-aws-warning
         log-aws-info
         log-aws-debug)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define/contract/provide (param<? a b)
  (-> (list/c symbol? string?) (list/c symbol? string?) boolean?)
  (string<? (symbol->string (car a))
            (symbol->string (car b))))

(module+ test
  (check-true  (param<? '(a "a") '(b "b")))
  (check-false (param<? '(b "b") '(a "a")))
  (check-false (param<? '(a "a") '(a "a"))))
