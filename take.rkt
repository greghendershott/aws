#lang racket

(provide in-take
         filter-take)

(module+ test
  (require rackunit))

;; Background: Functions like `hash', `hash-set*', `dict-set*' and so
;; on take argument couples as alternating arguments rather than a
;; cons pair. Using such functions can be refreshing, as you can type
;; simply (hash k0 v0 k1 v1) instead of all the dots and parens with
;; #hash([k0 . v0][k1 v2]). Using such functions is nice, but defining
;; them is a bit awkward -- unless you have in-take-list.  It lets you
;; sequence a flat list of items in groups of N, much like using
;; `take' and `drop'.
;;
;; Likewise, this makes it easy to write simple flat wrappers. for example
;; here is a `hash'-like initializer for association lists:
;;
;; (define (alist . xs)
;;   (for/list ([(k v) (in-take-list xs 2)])
;;     (cons k v)))
;;
;; Although the motivation for this was lists of couples, it supports
;; triples, quadruples -- any group size.


;; (in-take seq n) is a way to take `n' elements at a time from
;; `seq'. For instance with n=2 you will get successive couples of
;; elements, with n=3 you get triples, and so on.  `n' defaults to 2.
;; If there aren't exactly `n' elements at the end of the list, `fill'
;; is called. `fill' defaults to a procedure that raises an error, but
;; you may supply a procedure that "fills in missing values".

(struct
 take-list-iterator (xs n fill)
 #:methods gen:stream
 [(define (stream-empty? iter)
    (match-define (take-list-iterator xs n fill) iter)
    (or (empty? xs)                 ;exactly empty
        (and (< (length xs) n)      ;empty-ish, so...
             (begin (fill 0) #f)))) ;not empty unless `fill' raises exc
  (define (stream-first iter)
    (match-define (take-list-iterator xs n fill) iter)
    (define len (length xs))
    (define took (min n len))
    (apply values (append (take xs took) (for/list ([i (in-range took n)])
                                           (fill i)))))
  (define (stream-rest iter)
    (match-define (take-list-iterator xs n fill) iter)
    (take-list-iterator (drop xs (min n (length xs))) n fill))])

(struct take-list (xs n fill)
        #:property prop:sequence
        (lambda (c)
          (match-define (take-list xs n fill) c)
          (take-list-iterator xs n fill)))

(define (make-fill who n)
  (lambda (_)
    (error who "list not multiple of ~a items" n)))

(define fill/c (exact-nonnegative-integer? . -> . any/c))

(define/contract (in-take seq [n 2] [fill (make-fill 'in-take-list n)])
  ((sequence?) (exact-positive-integer? fill/c) . ->* . take-list?)
  (take-list (sequence->list seq) n fill))

(module+ test
  (test-case
   "in-take"
   ;; Sequence is multiple of take size
   (check-equal? (for/list ([(k v) (in-take (list 'a "1" 'b "2"))])
                   (cons k v))
                 '([a . "1"][b . "2"]))
   (check-equal? (for/list ([(k v) (in-take (vector 'a "1" 'b "2"))])
                   (cons k v))
                 '([a . "1"][b . "2"]))
   (check-equal? (for/list ([(k v) (in-take "a1b2")])
                   (cons k v))
                 '([#\a . #\1][#\b . #\2]))
   ;; Missing values raising an error (the default `fill')
   (check-exn exn:fail?
              (lambda () (for/list ([(k v) (in-take '(a "1" b "2" c) 2)])
                      (cons k v))))
   ;; Missing values filled in
   (check-equal? (for/list ([(k v) (in-take '(a "1" b "2" c) 2
                                            (const "FILL"))])
                   (cons k v))
                 '([a . "1"][b . "2"][c . "FILL"]))
   ;; Fill function is passed expected "index"
   (check-equal? (for/list ([(a b c d e) (in-take '(0 1) 5 values)])
                   (list a b c d e))
                 '((0 1 2 3 4))) ;fill was called with 2 3 4
   ;; Taking the same list with different take-sizes.
   (define test-xs (list 0 1 2 3 4 5 6 7 8 9))
   (define (test-take n)
     (for/list ([ts (in-values-sequence (in-take test-xs n))])
       ts))
   (check-equal? (test-take 1)
                 '((0) (1) (2) (3) (4) (5) (6) (7) (8) (9)))
   (check-equal? (test-take 2)
                 '((0 1) (2 3) (4 5) (6 7) (8 9)))
   (check-equal? (test-take 5)
                 '((0 1 2 3 4) (5 6 7 8 9)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Given a sequence, return a sequence containing only each group of `n'
;; elements for which `pred?' is true.

(define/contract (filter-take pred? seq [n 2]
                              [fill (make-fill 'in-take n)])
  ((procedure? sequence?) (exact-positive-integer? fill/c) . ->* . list?)
  (for/fold ([ys '()])
            ([ts (in-values-sequence (in-take seq n fill))])
    (cond [(apply pred? ts) (append ys ts)]
          [else ys])))

(module+ test
  (test-case
   "filter-take-list"
   (check-equal? (filter-take (lambda (a b) b) '(a 1 b #f c 3) 2)
                 '(a 1 c 3))
   (check-equal? (filter-take (const #f) '(1 2 3 4) 2) '())
   (check-equal? (filter-take (const #t) '() 2) '())))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

