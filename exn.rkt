#lang racket

(require (planet gh/http/request)
         (planet gh/http/head)
         net/head
         json
         xml
         "util.rkt"
         )

(struct exn:fail:aws exn:fail
        (http-code
         http-text
         aws-code
         aws-message)
        #:transparent)
(provide (struct-out exn:fail:aws))

;; Convenience: Given an input-port and response headers, check the
;; headers. If certain HTTP status codes where AWS returns an XML
;; response regarding an error, read the XML response from the port
;; and raise an exn:fail:aws constructed with info from the response.
;; Otherwise, return the same header string it was passed; and the
;; caller may read the response entity from the port itself.
;;
;; NOTE: This does NOT close the input port `p` before raising an
;; exception. It assumes you are using `call-request` or using
;; `dynamic-wind` or other exception handling, or a custodian -- or
;; whatever -- to make sure the port is closed.
(define/contract/provide (check-response p h)
  (input-port? string? . -> . string? #| OR raises exn:fail:aws |# )
  (define http-code (extract-http-code h))
  (define http-text (extract-http-text h))
  (match http-code
    ;; Codes we return and expect client to deal with.
    [200 h]
    [201 h]
    [202 h]
    [204 h]
    [206 h]
    [301 h]
    [302 h]
    [307 h] ;Temporary Redirect. Try again at Location header.
    ;; Codes for which we should raise an exception.
    [else
     (define x (read-entity/bytes p h))
     (raise (header&response->exn:fail:aws h x (current-continuation-marks)))]))

(define/contract/provide (header&response->exn:fail:aws h e ccm)
  (string? (or/c bytes? xexpr?) continuation-mark-set? . -> . exn:fail:aws?)
  (log-debug (format "~a ~a" h e))
  (define http-code (extract-http-code h))
  (define http-text (extract-http-text h))
  (cond
   [(and (bytes? e)
         (equal? "application/json" (extract-field "Content-Type" h)))
    (define js (bytes->jsexpr e))
    (define aws-code (hash-ref js 'code ""))
    (define aws-msg (hash-ref js 'message ""))
    (exn:fail:aws (format "HTTP ~a \"~a\". AWS Code=\"~a\" Message=\"~a\""
                          http-code http-text aws-code aws-msg)
                  ccm
                  ;; fields exn:fail:aws adds to exn:fail
                  http-code
                  http-text
                  aws-code
                  aws-msg)]
   [(bytes? e)
    (match e
      [(regexp "<Code>(.*?)</Code>.*?<Message>(.*?)</Message>"
               (list _ aws-code aws-msg))
       (exn:fail:aws (format "HTTP ~a \"~a\". AWS Code=\"~a\" Message=\"~a\""
                             http-code http-text aws-code aws-msg)
                     ccm
                     ;; fields exn:fail:aws adds to exn:fail
                     http-code
                     http-text
                     aws-code
                     aws-msg)]
      [else
       (exn:fail:aws (format "HTTP ~a \"~a\"." http-code http-text)
                     ccm
                     ;; fields exn:fail:aws adds to exn:fail
                     http-code
                     http-text
                     #f
                     h)])]
   [(xexpr? e)
    (define aws-code (first-tag-value e 'Code))
    (define aws-msg (first-tag-value e 'Message))
    (exn:fail:aws (format "HTTP ~a \"~a\". AWS Code=\"~a\" Message=\"~a\""
                          http-code http-text aws-code aws-msg)
                  ccm
                  ;; fields exn:fail:aws adds to exn:fail
                  http-code
                  http-text
                  aws-code
                  aws-msg)]))
