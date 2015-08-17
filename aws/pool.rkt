#lang racket/base

(require racket/async-channel
         racket/contract/base
         racket/contract/region)

(provide (struct-out pool)
         make-worker-pool
         delete-worker-pool
         with-worker-pool
         add-job
         get-results)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This probably should get moved to a general-purpose utility library.
;;
;; Perhaps someday it should get interface compatible variations that
;; use (say) places as well as threads.

(struct pool
        (threads  ;(listof thread?)
         todo     ;async-channel? expected to contain (-> any/c) job procs
         done     ;async-channel? expected to contain any/c results
         ))
;; Don't need to provide this; opaque to users.

(define/contract (make-worker-pool num-threads)
  (exact-positive-integer? . -> . pool?)
  (define todo (make-async-channel))
  (define done (make-async-channel))
  (define (worker-thread)
    (let loop ()
      (let ([p (async-channel-get todo)])
        (cond [(procedure? p)
               (async-channel-put done (p))
               (loop)]
              [else
               (error 'worker-thread "expected procedure, got" p)]))))
  (pool (for/list ([n (in-range num-threads)])
          (thread worker-thread))
        todo
        done))

(define/contract (delete-worker-pool p)
  (pool? . -> . any)
  (for ([t (in-list (pool-threads p))])
    (kill-thread t)))

;; This is safer than using make-worker-pool and
;; delete-worker-pool. If there is an exception, delete-worker-pool
;; will still be called to clean up.
(define/contract (with-worker-pool num-threads proc)
  (exact-positive-integer? (pool? . -> . any) . -> . any)
  (define p (make-worker-pool num-threads))
  (dynamic-wind (lambda () (void))
                (lambda () (proc p))
                (lambda () (delete-worker-pool p))))

;; A job is a procedure that takes no arguments, and returns any/c,
;; which is put the done channel.
(define/contract (add-job pool proc)
  (pool? (-> any/c) . -> . any)
  (async-channel-put (pool-todo pool) proc))

;; Gets the specified number of results, blocking until they are
;; available.
(define/contract (get-results p n)
  (pool? exact-nonnegative-integer? . -> . any/c)
  (let loop ([xs '()]
             [n n])
    ;; (displayln (tr "get-results" n xs))
    (cond [(zero? n) xs]
          [else (loop (cons (async-channel-get (pool-done p)) xs)
                      (sub1 n))])))

(module+ test
  (require rackunit)
  (test-case
   "worker pool"
   (define (try num-threads num-jobs)
     (define results
       (with-worker-pool
        num-threads
        (lambda (pool)
          (for ([i (in-range num-jobs)])
            (add-job pool (lambda () (sleep (random)) i)))
          (get-results pool num-jobs))))
     (check-equal? (sort results <)
                   (for/list ([i (in-range num-jobs)])
                     i)))
   ;; Try a few permutations of threads and jobs.
   (try 4 20)
   (try 1 10)
   (try 10 1)
   ))
