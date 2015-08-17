#lang racket/base

(require net/base64
         racket/contract/base
         racket/contract/region
         racket/file
         racket/match
         sha
         "util.rkt")

;; These parameters hold the AWS keys. Set them before using functions
;; in this module, for example by caling `read-aws-keys'.
(define public-key (make-parameter ""))
(define private-key (make-parameter ""))
(provide public-key
         private-key)

(define/provide (read-keys [file
                            (build-path (find-system-path 'home-dir)
                                        ".aws-keys")])
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

(define/provide (ensure-have-keys)
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

(define/contract (shaX-encode str f)
  (string? (bytes? bytes? . -> . bytes?) . -> . string?)
  (match (bytes->string/utf-8
          (base64-encode (f (string->bytes/utf-8 (private-key))
                            (string->bytes/utf-8 str))))
    [(regexp #rx"^(.*)\r\n$" (list _ x)) x] ;kill \r\n added by base64-encode
    [s s]))

(define/contract/provide (sha1-encode str)
  (string? . -> . string?)
  (shaX-encode str hmac-sha1))

(define/contract/provide (sha256-encode str)
  (string? . -> . string?)
  (shaX-encode str hmac-sha256))
