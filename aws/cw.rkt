#lang racket/base

(require (for-syntax racket/base)
         http/request
         racket/bool
         racket/contract/base
         racket/contract/combinator
         racket/contract/region
         racket/dict
         racket/list
         racket/match
         racket/string
         xml/xexpr
         "keys.rkt"
         "post.rkt"
         "sigv4.rkt"
         "util.rkt")

(provide cw-endpoint
         cw-region)

(define cw-endpoint
 (make-parameter (endpoint "monitoring.us-east-1.amazonaws.com" #t)))
(define cw-region
 (make-parameter "us-east-1"))

;; core procedure to make CloudWatch requests
(define/contract (cw params [result-proc values])
  (((listof (list/c symbol? string?)))
   ((xexpr? . -> . list?))
   . ->* . list?)
  (ensure-have-keys)
  (let* ([uri (endpoint->uri (cw-endpoint) "/")]
         [heads (hasheq 'Host (endpoint-host (cw-endpoint))
                        'Date (seconds->gmt-8601-string 'basic (current-seconds))
                        'Content-Type "application/x-www-form-urlencoded; charset=utf-8")]
         [params (sort (cons `(Version "2010-08-01") params)
                       param<?)]
         [post-data (string->bytes/utf-8 (dict->form-urlencoded params))]
         [heads (dict-set* heads
                           'Authorization
                           (aws-v4-authorization "POST"
                                                 uri
                                                 heads
                                                 (sha256-hex-string post-data)
                                                 (cw-region)
                                                 "monitoring"))]
         [x (post-with-retry uri params heads)])
    (append (result-proc x)
            (match (se-path* '(NextToken) x)
              [#f '()]
              [token (cw (set-next-token params token)
                         result-proc)]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (secs->str s)
  (seconds->gmt-8601-string 'T/Z s))

(define (str->secs s)
  (cond [(false? s) 0]
        [else (gmt-8601-string->seconds s)]))

(define (string->boolean s)
  (cond [(string-ci=? "true" s) #t]
        [(string-ci=? "false" s) #f]
        [else
         (define n (string->number s))
         (cond [(false? n)
                (error 'string->boolean
                       "cannot convert string \"~a\" to a boolean" s)]
               [else (not (zero? n))])]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; nuke whitespace from xexpr?
;;
;; I feel like I'm overlooking something obvious about how to read
;; this in the first place, but even after perusing Racket docs I'm
;; stumped.

;; xexpr? -> xexpr?
(define (nuke-ws xs)
  (filter-map ws xs))

;; any/c -> (or/c any/c #f)
(define (ws x)
  (cond [(list? x) (nuke-ws x)]
        [(string? x)
         (match x
           [(pregexp "^\\s*$") #f]
           [else x])]
        [else x]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define unit/c
  (or/c 'None
        'Percent
        'Count
        'Seconds 'Microseconds 'Milliseconds
        'Bytes 'Kilobytes 'Megabytes 'Gigabytes 'Terabytes
        'Bits 'Kilobits 'Megabits 'Gigabits 'Terabits
        'Count/Second
        'Bytes/Second 'Kilobytes/Second 'Megabytes/Second
          'Gigabytes/Second 'Terabytes/Second
        'Bits/Second 'Kilobits/Second 'Megabits/Second
          'Gigabits/Second 'Terabits/Second
        ))

(define period/c
  (make-flat-contract #:name 'non-zero-multiple-of-60
                      #:first-order (λ (x)
                                      (and (>= x 60)
                                           (zero? (modulo x 60))))))

(define statistic/c
  (or/c 'Sum 'Average 'Maximum 'Minimum 'SampleCount))

(define dimensions/c
  (listof (list/c symbol? string?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 2->1: In response XML, each dimension is two elements, <Name> and
;; <Value>. Combine them into a single dimension/c.
(define/contract (xexpr->dimensions/c x)
  (xexpr? . -> . dimensions/c)
  (for/list ([x (in-list (se-path*/elements '(Dimensions member) x))])
      (list (string->symbol (se-path* '(Name) x))
            (se-path* '(Value) x))))

;; 1->2: In HTTP request params each dimension/c needs to be split
;; into two query parameters, Name=x and Value=y.
(define/contract (dimensions/c->params xs)
  (dimensions/c
   . -> .
   (listof (list/c symbol? string?)))
  (for/fold ([xs '()])
      ([x (in-list xs)]
       [i (in-naturals 1)])
    (match-define (list name value) x)
    (append
     xs
     (list (list (string->symbol (format "Dimensions.Member.~a.Name" i))
                 (symbol->string name))
           (list (string->symbol (format "Dimensions.Member.~a.Value" i))
                 value)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct metric (name             ;string?
                namespace        ;string?
                dimensions       ;dimensions/c
                ) #:transparent)
(provide (struct-out metric))

(define/contract/provide (list-metrics #:metric-name [metric-name #f]
                                       #:namespace [namespace #f]
                                       #:dimensions [dimensions '()])
  (()
   (#:metric-name (or/c #f string?)
    #:namespace (or/c #f string?)
    #:dimensions dimensions/c)
   . ->* . (listof metric?))
  (cw `((Action "ListMetrics")
        ,@(if metric-name `((MetricName ,metric-name)) `())
        ,@(if namespace `((Namespace ,namespace)) `())
        ,@(dimensions/c->params dimensions))
      (λ (x)
        ;; Only get 'member elements that are direct kids of 'Metrics.
        (for/list ([x (in-list (se-path*/elements '(Metrics member) (nuke-ws x)))])
            (metric (se-path* '(MetricName) x)
                    (se-path* '(Namespace) x)
                    (xexpr->dimensions/c x))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct datum
        (
         metric-name         ;string?
         value               ;(or/c #f number?)
         min                 ;(or/c #f number?)
         max                 ;(or/c #f number?)
         sum                 ;(or/c #f number?)
         sample-count        ;(or/c #f number?)
         unit                ;unit/c
         timestamp           ;exact-integer?
         dimensions          ;dimensions/c
         ) #:transparent)
(provide (struct-out datum))

(define/contract/provide (put-metric-data namespace data)
  (string? (non-empty-listof datum?) . -> . void?)
  (cw `((Action "PutMetricData")
        (Namespace ,namespace)
        ,@(for/fold ([xs '()])
              ([x (in-list data)]
               [n (in-naturals 1)])
            (define (do k v)
              (list (string->symbol (format "MetricData.member.~a.~a" n k))
                    v))
            (define (do/stat k v)
              (if v
                  `((,(string->symbol
                       (format "MetricData.member.~a.StatisticSet.~a" n k))
                     ,(number->string v)))
                   `()))
            (match-define (datum metric-name
                                        value
                                        min max sum sample-count
                                        unit
                                        timestamp
                                        dimensions) x)
            (append
             xs
             (list (do "MetricName" metric-name)
                   (do "Value" (number->string value))
                   (do "Unit" (symbol->string unit))
                   (do "Timestamp" (secs->str timestamp)))
             (do/stat "Minimum" min)
             (do/stat "Maximum" max)
             (do/stat "Sum" sum)
             (do/stat "SampleCount" sample-count)
             (for/fold ([xs/d '()])
                 ([x/d (in-list dimensions)]
                  [n/d (in-naturals 1)])
               (define (do/d k/d v/d)
                 (list (string->symbol
                        (format "Metric.Data.member.~a.Dimensions.member.~a.~a"
                                n n/d k/d))
                       (format "~a" v/d)))
               (match-define (list name value) x/d)
               (append
                xs/d
                (list (do/d "Name" name)
                      (do/d "Value" value))))))))
  (void))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define/contract/provide (get-metric-statistics #:metric-name metric-name
                                                #:namespace namespace
                                                #:statistics statistics
                                                #:unit unit
                                                #:start-time start-time
                                                #:end-time end-time
                                                #:period [period 60]
                                                #:dimensions [dimensions '()])
  (->* (#:metric-name string?
        #:namespace string?
        #:statistics (non-empty-listof statistic/c)
        #:unit unit/c
        #:start-time exact-integer?
        #:end-time exact-integer?)
       (#:period period/c
        #:dimensions dimensions/c)
       (listof datum?))
  (cw `((Action "GetMetricStatistics")
        (MetricName ,metric-name)
        (Namespace ,namespace)
        ,@(for/list ([x (in-list statistics)]
                     [n (in-naturals 1)])
            `(,(string->symbol (format "Statistics.member.~a" n))
              ,(symbol->string x)))
        (Unit ,(symbol->string unit))
        (Period ,(number->string period))
        (StartTime ,(secs->str start-time))
        (EndTime ,(secs->str end-time))
        ,@(dimensions/c->params dimensions))
      (λ (xpr)
        (for/list ([x (in-list (se-path*/elements '(Datapoints member) (nuke-ws xpr)))])
          (define (get sym [f values])
            (match (se-path* (list sym) x)
              [#f #f]
              [v (f v)]))
          (datum metric-name
                 #f
                 (get 'Minimum string->number)
                 (get 'Maximum string->number)
                 (get 'Sum string->number)
                 (get 'SampleCount string->number)
                 (get 'Unit string->symbol)
                 (get 'Timestamp str->secs)
                 (get 'Dimensions))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct alarm
        (
         name
         description
         arn
         configuration-updated-timestamp
         metric-name
         namespace
         threshold
         comparison-operator
         alarm-actions
         ok-actions
         insufficient-data-actions
         state-value
         state-reason
         state-reason-data
         state-updated-timestamp
         period
         actions-enabled
         evaluation-periods
         statistic
         dimensions
         ) #:transparent)
(provide (struct-out alarm))

(define (xexpr->alarms xpr)
  ;; xexpr? -> (listof metric-alarm?)
  ;; Only get 'member elements that are direct kids of 'MetricAlarms
  (for/list ([x (in-list (se-path*/elements '(MetricAlarms member) (nuke-ws xpr)))])
    (define (get sym [f values])
      (match (se-path* (list sym) x)
        [#f #f]
        [v (f v)]))
    (alarm
     (get 'AlarmName)
     (get 'AlarmDescription)
     (get 'AlarmArn)
     (get 'AlarmConfigurationUpdatedTimestamp str->secs)
     (get 'MetricName)
     (get 'Namespace)
     (get 'Threshold string->number)
     (get 'ComparisonOperator)
     (get 'AlarmActions)
     (get 'OKActions)
     (get 'InsufficientDataActions)
     (get 'StateValue)
     (get 'StateReason)
     (get 'StateReasonData)
     (get 'StateUpdatedTimestamp str->secs)
     (get 'Period string->number)
     (get 'ActionsEnabled string->boolean)
     (get 'EvaluationPeriods string->number)
     (get 'Statistic)
     (xexpr->dimensions/c x)
     ;; (first-tag-value x 'Dimensions '())
     )))

(define/contract/provide (describe-alarms
                          #:alarm-name-prefix [alarm-name-prefix #f]
                          #:alarm-names [alarm-names '()]
                          #:state-value [state-value #f])
  (()
   (#:alarm-name-prefix (or/c #f string?)
    #:alarm-names (listof string?)
    #:state-value (or/c #f 'OK 'ALARM 'INSUFFICIENT_DATA))
   . ->* . (listof alarm?))
  (cw `((Action "DescribeAlarms")
        ,@(if alarm-name-prefix `((AlarmNamePrefx ,alarm-name-prefix)) `())
        ,@(for/list ([x (in-list alarm-names)]
                     [n (in-naturals 1)])
              `(,(string->symbol (format "AlarmNames.member.~a" n))
                ,x))
        ,@(if state-value `((StateValue ,(symbol->string state-value))) `()))
      xexpr->alarms))

(define/contract (describe-alarms-for-metric #:metric-name metric-name
                                             #:namespace namespace
                                             #:period [period #f]
                                             #:statistic [statistic #f]
                                             #:unit [unit #f]
                                             #:dimensions [dimensions '()])
  ((#:metric-name string?
    #:namespace string?)
   (#:statistic statistic/c
    #:unit unit/c
    #:period (or/c #f period/c)
    #:dimensions dimensions/c)
   . ->* . (listof alarm?))
  (cw `((Action "DescribeAlarmsForMetric")
        (MetricName ,metric-name)
        (Namespace ,namespace)
        ,@(if statistic `((Statistic ,(symbol->string statistic))) `())
        ,@(if unit `((Unit ,(symbol->string unit))) `())
        ,@(if period `((Period ,(number->string period))) `())
        ,@(dimensions/c->params dimensions))
      xexpr->alarms))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct alarm-history
        (timestamp              ;exact-integer?
         item-type              ;string?
         name                   ;string?
         data                   ;xexpr? (JSON)
         summary                ;string?
         )
        #:transparent)

(define/contract/provide (describe-alarm-history #:alarm-name [alarm-name #f]
                                                 #:start-date [start-date #f]
                                                 #:end-date [end-date #f]
                                                 #:history-type [history-type #f])
  (()
   (#:alarm-name (or/c #f string?)
    #:start-date (or/c #f exact-integer?)
    #:end-date (or/c #f exact-integer?)
    #:history-type (or/c #f 'ConfigurationUpdate 'StateUpdate 'Action))
   . ->* . (listof alarm-history?))
  '()
  (cw `((Action "DescribeAlarmHistory")
        ,@(if alarm-name `((AlarmName ,alarm-name)) `())
        ,@(if start-date `((StartDate ,(secs->str start-date))) `())
        ,@(if end-date `((EndDate ,(secs->str end-date))) `())
        ,@(if history-type `((HistoryType ,(format "~a" history-type))) `()))
      (λ (x)
        (for/list ([x (in-list (se-path*/elements '(AlarmHistoryItems member) (nuke-ws x)))])
          (define (get sym [f values])
            (match (se-path* (list sym) x)
              [#f #f]
              [v (f v)]))
          (alarm-history (get 'Timestamp str->secs)
                         (get 'HistoryItemType)
                         (get 'AlarmName)
                         ;; HistoryData is a bunch of JSON. Punt on
                         ;; parsing that; just return as-is.
                         (string-join (se-path*/list '(HistoryData) x) "")
                         (get 'HistorySummary))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test

#;
(list-metrics #:metric-name "CPUUtilization"
              #:namespace "AWS/EC2"
              ;; #:dimensions `((InstanceId "i-cfeb8ba4"))
              )

#;
(map metric-alarm-name
     (describe-alarms #:alarm-names `("www-i-acd3f2c3-high-CPU")))

#;
(map metric-alarm-name
     (describe-alarms #:state-value 'OK))

;; Why is this working but returning no results?
#;
(describe-alarms-for-metric #:metric-name "CPUUtilization"
                            #:namespace "AWS/EC2"
                            ;; #:statistic 'Average
                            ;; #:unit 'Percent
                            )

#;
(describe-alarm-history)


#|
(get-metric-statistics #:metric-name "testmetric"
                       #:namespace "testnamespace"
                       #:statistics '(Average Sum Minimum Maximum SampleCount)
                       #:unit 'Count
                       #:period (* 60 60) ;1 hour
                       #:start-time (- (current-seconds) (* 7 24 60 60))
                       #:end-time (current-seconds)
                       ;;#:dimensions `((InstanceId "i-cfeb8ba4"))
                       )
|#

(module+ test
  (require rackunit
           "tests/data.rkt")
  (define (do-test)
    ;; describe-alarms
    (define xs (describe-alarms))
    (check-equal? (describe-alarms #:alarm-names (map alarm-name xs))
                  xs)

    ;; put/list/get metric data
    (define test-unit 'Percent)
    (define test-dimensions `((FakeDimensionName "FakeDimensionValue")))

    (define end (current-seconds))
    (define beg (- end (* 60 101)))        ;101 mintutes earlier
    (define xs-put
      (for/list ([n (in-range 0 101 1)]
                 [sc (in-range beg end 60)])
        (datum (test/metric)
               n
               #f #f #f #f
               test-unit
               sc
               test-dimensions)))

    ;; CW doesn't want > 20 at once. So do in batches of 20, which is
    ;; good to exercise our handling of that.
    (let loop ([xs xs-put])
      (define len (length xs))
      (define this (min len 20))
      (define next (- len this))
      (unless (zero? this)
        (put-metric-data (test/namespace) (take xs this))
        (unless (zero? next)
          (loop (take-right xs next)))))

    ;; First time, may take awhile to show up
    (let loop ([tries 8])
      (unless (zero? tries)
        (when (empty? (list-metrics #:namespace (test/namespace)))
          (sleep 15)
          (loop (sub1 tries)))))

    (define m (list (metric (test/metric) (test/namespace) '())))
    (check-equal? (list-metrics #:namespace (test/namespace)) m)
    (check-equal? (list-metrics #:metric-name (test/metric)) m)

    (define xs-get
      (get-metric-statistics #:metric-name (test/metric)
                             #:namespace (test/namespace)
                             #:unit test-unit
                             #:statistics '(Sum Average Minimum Maximum
                                                SampleCount)
                             #:period 60
                             #:start-time (- (current-seconds) (* 24 60 60))
                             #:end-time (current-seconds)
                             ;; #:dimensions test-dimensions
                             ))
    (check-true (not (empty? xs-get)))
    (check-equal? (remove-duplicates (map datum-metric-name xs-get))
                  (list (test/metric)))
    (check-equal? (remove-duplicates (map datum-unit xs-get))
                  (list test-unit))
    ;; datum-value should always be #f when returned from
    ;; get-metric-statistics
    (check-equal? (remove-duplicates (map datum-value xs-get))
                  (list #f))
    ;; We specified all the statistics in #:statistics above, so make
    ;; sure all are non-#f
    (check-not-equal? (remove-duplicates (append
                                          (map datum-min xs-get)
                                          (map datum-max xs-get)
                                          (map datum-sum xs-get)
                                          (map datum-sample-count xs-get)))
                      (list #f)))
  (when (test-data-exists?)
    (for ([region '("us-east-1" "eu-central-1")])
      (parameterize ([cw-region region]
                     [cw-endpoint (endpoint (string-append "monitoring."
                                                           region
                                                           ".amazonaws.com")
                                            #f)])
        (do-test)))))
