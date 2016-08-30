#lang at-exp racket/base

(require racket/async-channel
         racket/contract/base
         racket/contract/region
         racket/format
         "util.rkt")

(provide pool?
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
   done))   ;async-channel? expected to contain any/c results

(define/contract (make-worker-pool num-threads #:retry-delay [retry-delay 1000])
  (->* (exact-positive-integer?)
       (#:retry-delay exact-positive-integer?)
       pool?)
  (define todo (make-async-channel))
  (define done (make-async-channel))
  (define (worker-thread)
    (let run-job ()
      (define cust (make-custodian))
      (parameterize ([current-custodian cust])
        (define f (async-channel-get todo))
        (with-handlers ([exn:fail?
                         (λ (e)
                           (log-aws-warning
                            @~a{Worker got exn:
                                  @e;
                                  Will retry job later.})
                           ;; Note: Composing `wait-at-least' doubles
                           ;; the delay, i.e. backoff: Good.
                           (async-channel-put todo (wait-at-least f retry-delay)))])
          (async-channel-put done (f))))
      (custodian-shutdown-all cust)
      (run-job)))
  (pool (for/list ([n (in-range num-threads)])
          (thread worker-thread))
        todo
        done))

(define ((wait-at-least f msec-delay))
  (sync (alarm-evt (+ (current-inexact-milliseconds) msec-delay)))
  (f))

(define/contract (delete-worker-pool p)
  (-> pool? any)
  (for ([t (in-list (pool-threads p))])
    (kill-thread t)))

;; This is safer than using make-worker-pool and
;; delete-worker-pool. If there is an exception, delete-worker-pool
;; will still be called to clean up.
(define/contract (with-worker-pool num-threads proc)
  (-> exact-positive-integer? (-> pool? any) any)
  (define p (make-worker-pool num-threads))
  (dynamic-wind (λ () (void))
                (λ () (proc p))
                (λ () (delete-worker-pool p))))

;; A job is a procedure that takes no arguments, and returns any/c,
;; which is put the done channel.
(define/contract (add-job pool proc)
  (-> pool? (-> any/c) any)
  (async-channel-put (pool-todo pool) proc))

;; Gets the specified number of results, blocking until they are
;; available.
(define/contract (get-results p n)
  (-> pool? exact-nonnegative-integer? list?)
  (for/list ([_ (in-range n)])
    (async-channel-get (pool-done p))))

(module+ test
  (require rackunit)
  (define (try num-threads num-jobs)
    (define results
      (with-worker-pool
        num-threads
        (λ (pool)
          (for ([i (in-range num-jobs)])
            (add-job pool (λ ()
                            ;; Excercise the exn re-run feature 1 in 10 times
                            (when (zero? (random 10))
                              (error 'example-error "example error"))
                            (sleep (random))
                            i)))
          (get-results pool num-jobs))))
    (check-equal? (sort results <)
                  (for/list ([i (in-range num-jobs)])
                    i)))
   ;; Try a few permutations of threads and jobs.
   (try 4 20)
   (try 1 10)
   (try 10 1))
