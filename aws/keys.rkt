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
         security-token
         credentials-from-file!
         (rename-out [credentials-from-file! read-keys/aws-cli])
         aws-cli-credentials
         aws-cli-profile
         credentials-from-environment!
         sha256-encode
         credentials-from-ec2-instance!
         (rename-out [credentials-from-ec2-instance! use-iam-ec2-credentials!])
         ensure-ec2-instance-credentials-and-add-token-header
         read-keys
         ensure-have-keys)

(define public-key (make-parameter ""))
(define private-key (make-parameter ""))
(define security-token (make-parameter #f))

(define aws-cli-credentials
  (make-parameter (or (getenv "AWS_SHARED_CREDENTIALS_FILE")
                      (build-path (find-system-path 'home-dir) ".aws" "credentials"))))
(define aws-cli-profile
  (make-parameter (or (getenv "AWS_DEFAULT_PROFILE") "default")))

(define (credentials-from-file!)
  (define (get/set key param)
    (match (get-profile-string (file->lines (aws-cli-credentials) #:mode 'text)
                               (aws-cli-profile)
                               key)
      [#f (error 'read-keys/aws-cli
                 "could not find key ~v in section ~v of ~v"
                 key (aws-cli-profile) (aws-cli-credentials))]
      [v (param v)]))
  (get/set "aws_access_key_id" public-key)
  (get/set "aws_secret_access_key" private-key))

(define (credentials-from-environment!)
  (define (get/set env-var param)
    (match (getenv env-var)
      [#f (error 'read-keys-and-token/environment
                 "could not find environment variable ~v"
                 env-var)]
      [v (param v)]))
  (get/set "AWS_ACCESS_KEY_ID"     public-key)
  (get/set "AWS_SECRET_ACCESS_KEY" private-key)
  (get/set "AWS_SESSION_TOKEN"     security-token))

(define (get-profile-string lines section key)
  (let find-section ([lines lines])
    (match lines
      [(list) #f]
      [(cons (pregexp "^ *\\[(.+?)\\] *$" (list _ (== section))) more)
       (let find-key ([lines more])
         (match lines
           [(list) #f]
           [(cons (pregexp "^ *(.+?) *= *(.+?) *$" (list _ (== key) value)) _)
            value]
           [(cons _ more) (find-key more)]))]
      [(cons _ more) (find-section more)])))

;; DEPRECATED
(define (read-keys [file (build-path (find-system-path 'home-dir) ".aws-keys")])
  (match (file->lines file #:mode 'text #:line-mode 'any)
    ;; old format that Amazon uses for their CL tools:
    [(list* (regexp #rx"^(?i:AWSAccessKeyId)=(.*)$" (list _ public))
            (regexp #rx"^(?i:AWSSecretKey)=(.*)$" (list _ private))
            _)
     (public-key public)
     (private-key private)]
    ;; for backward compatability my old way, just each key on own line:
    [(list* public private _)
     (public-key public)
     (private-key private)]
    [_ (error 'read-keys
              (string-append
               "First two lines of file must be:\n"
               "AWSAccessKeyId=<key>\n"
               "AWSSecretKey=<key>\n"))]))

;; DEPRECATED
(define (ensure-have-keys)
  (define (keys-blank?)
    (or (string=? "" (public-key))
        (string=? "" (private-key))))
  (when (keys-blank?)
    (with-handlers ([exn:fail? (λ _ (credentials-from-file!))])
      (read-keys)))
  (when (keys-blank?)
    (error 'ensure-have-keys
           "Set the parameters `public-key` and `private-key`. See the `credentials-from-xxx!` functions.")))

(define/contract (sha256-encode str)
  (-> string? string?)
  (match (bytes->string/utf-8
          (base64-encode (sha256-encode (string->bytes/utf-8 (private-key))
                                        (string->bytes/utf-8 str))))
    [(regexp #rx"^(.*)\r\n$" (list _ x)) x] ;kill \r\n added by base64-encode
    [s s]))


;;; Get credentials from EC2 instance meta-data

;; Note: These aren't parameters because parameters are per-thread --
;; whereas we'll need to update from one thread values for all
;; threads.
(define/contract iam-role (or/c #f string?) #f)
(define/contract ec2-instance-creds-expiration (or/c #f integer?) #f)
(define sema (make-semaphore 1))

(define (credentials-from-ec2-instance! v)
  (set! iam-role v)
  (ensure-ec2-instance-credentials))

(define (ensure-ec2-instance-credentials-and-add-token-header d)
  (ensure-ec2-instance-credentials)
  (add-token-header d))

(define (ensure-ec2-instance-credentials)
  (when iam-role
    (call-with-semaphore
     sema
     (λ ()
       (unless (and ec2-instance-creds-expiration
                    (< (+ (current-seconds) (* 5 60))
                       ec2-instance-creds-expiration))
         (define url
           (string->url
            (~a "http://169.254.169.254/latest/meta-data/iam/security-credentials/"
                iam-role)))
         (match (call/input-url url get-pure-port read-json)
           [(hash-table ['AccessKeyId     public]
                        ['SecretAccessKey private]
                        ['Token           token]
                        ['Expiration      (app gmt-8601-string->seconds exp)])
            (public-key public)
            (private-key private)
            (security-token token)
            (set! ec2-instance-creds-expiration exp)]))))))

(define (add-token-header d)
  (if (security-token)
      (dict-set d 'X-Amz-Security-Token (security-token))
      d))
