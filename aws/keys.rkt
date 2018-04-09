#lang racket/base

(require (only-in http
                  gmt-8601-string->seconds)
         json
         net/base64
         net/url
         racket/contract
         racket/dict
         racket/file
         racket/format
         racket/match
         sha
         "util.rkt")

(provide public-key
         private-key
         read-keys
         ensure-have-keys
         sha256-encode
         use-iam-ec2-credentials!
         ensure-ec2-instance-credentials-and-add-token-header)

(define public-key (make-parameter ""))
(define private-key (make-parameter ""))

(define (read-keys [file (build-path (find-system-path 'home-dir) ".aws-keys")])
  (match (file->lines file #:mode 'text #:line-mode 'any)
    ;; same format that Amazon uses for their CL tools:
    [(list (regexp #rx"^(?i:AWSAccessKeyId)=(.*)$" (list _ public))
           (regexp #rx"^(?i:AWSSecretKey)=(.*)$" (list _ private))
           _ ...)
     (public-key public)
     (private-key private)]
    ;; for backward compatability my old way, just each key on own line:
    [(list public
           private
           _ ...)
     (public-key public)
     (private-key private)]
    [else (error 'read-keys
                 (string-append
                  "First two lines of file must be:\n"
                  "AWSAccessKeyId=<key>\n"
                  "AWSSecretKey=<key>\n"))]))

(define (ensure-have-keys)
  (define (keys-blank?)
    (or (string=? "" (public-key))
        (string=? "" (private-key))))
  (when (keys-blank?)
    (read-keys))
  (when (keys-blank?)
    (error 'ensure-have-keys
           (string-append "Set the parameters `public-key' and "
                          "`private-key' to the AWS AccessKeyID "
                          "and SecretKey, respectively. "
                          "Tip: `(read-keys)' will read them "
                          "from a ~~/.aws-keys file."))))

(define/contract (sha256-encode str)
  (-> string? string?)
  (match (bytes->string/utf-8
          (base64-encode (sha256-encode (string->bytes/utf-8 (private-key))
                                        (string->bytes/utf-8 str))))
    [(regexp #rx"^(.*)\r\n$" (list _ x)) x] ;kill \r\n added by base64-encode
    [s s]))


;;; Optional: Get credentials from EC2 instance meta-data

(struct creds (public private token expires))

;; Note: These aren't parameters because parameters are per-thread --
;; whereas we'll need to update values from one thread for all
;; threads.
(define/contract iam-role (or/c #f string?) #f)
(define/contract the-creds (or/c #f creds?) #f)

(define (use-iam-ec2-credentials! v)
  (set! iam-role v))

(define (ensure-ec2-instance-credentials-and-add-token-header d)
  (let ([sema (make-semaphore 1)])
    (λ (d)
      (cond [iam-role
             (call-with-semaphore
              sema
              (λ ()
                (unless (and the-creds
                             (< (+ (current-seconds) (* 5 60))
                                (creds-expires the-creds)))
                  (set! the-creds (get-creds)))
                (public-key (creds-public the-creds))
                (private-key (creds-private the-creds))
                (dict-set d 'X-Amz-Security-Token (creds-token the-creds))))]
            [else d]))))

(define (get-creds)
  (define url
    (string->url
     (~a "http://169.254.169.254/latest/meta-data/iam/security-credentials/"
         iam-role)))
  (match (call/input-url url get-pure-port read-json)
    [(hash-table ['AccessKeyId     public]
                 ['SecretAccessKey private]
                 ['Token           token]
                 ['Expiration      expiration])
     (creds public private token (gmt-8601-string->seconds expiration))]))
