;; Copyright (c) 2012-2022 by Greg Hendershott.
;; SPDX-License-Identifier: BSD-2-Clause

#lang racket/base

(require racket/contract/base
         racket/contract/region
         racket/function
         racket/list
         racket/match
         xml/path
         xml/xexpr)

(provide (all-from-out xml/path)
         se-path*/elements)

(define (take<= xs n)
  (for/list ([_ (in-range n)]
             [x (in-list xs)])
    x))

;; Like se-path*/list, but returns the entire element not just its
;; body values. As such, also does not support a keyword as the final
;; element in the path.
(define/contract (se-path*/elements path xexpr)
  (-> (listof symbol?) xexpr/c (listof xexpr/c))
  (let* ([path (reverse path)] ;reverse to compare to path here
         [path-len (length path)])
    (let search-xexpr ([path-here '()]
                       [xexpr xexpr])
      (define (matched xexpr tag body)
        (let ([path-here (cons tag path-here)])
           (append (cond [(equal? (take<= path-here path-len) path)
                          (list xexpr)]
                         [else '()])
                   (append-map (curry search-xexpr path-here) body))))
      (match xexpr
        [(list-rest tag (list (list (? symbol?) (? string?)) ...) body)
         (matched xexpr tag body)]
        [(list-rest tag body)
         (matched xexpr tag body)]
        [_ '()]))))

(module+ test
  (require rackunit)
  (check-equal? (se-path*/elements '()
                                   '(a () (b () "b")))
                '((a () (b () "b"))
                  (b () "b")))
  (check-equal? (se-path*/elements '()
                                   '(a (b "b")))
                '((a (b "b"))
                  (b "b")))
  (check-equal? (se-path*/elements '(a)
                                   '(a () (b () "b")))
                '((a () (b () "b"))))
  (check-equal? (se-path*/elements '(a)
                                   '(a (b "b")))
                '((a (b "b"))))
  (check-equal? (se-path*/elements '(a b)
                                   '(a () (b () "b")))
                '((b () "b")))
  (check-equal? (se-path*/elements '(a b)
                                   '(a (b "b")))
                '((b "b")))
  (check-equal? (se-path*/elements '(a b)
                                   '(x ()
                                       (z ()
                                          (a () (b () "b0"))
                                          (a () (b () "b1")))))
                '((b () "b0")
                  (b () "b1")))
  (check-equal? (se-path*/elements '(a b)
                                   '(x
                                     (z
                                      (a (b "b0"))
                                      (a (b "b1")))))
                '((b "b0")
                  (b "b1"))))
