#lang racket

(require xml
         (planet gh/http/request)
         "util.rkt"
         "keys.rkt"
         "exn.rkt"
         "post.rkt"
         )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define cw-endpoint (make-parameter
                     (endpoint "monitoring.us-east-1.amazonaws.com" #t)))
(provide cw-endpoint)

;; core procedure to make CloudWatch requests
(define/contract (cw params [result-proc values])
  (((listof (list/c symbol? string?)))
   ((xexpr? . -> . list?))
   . ->* . list?)
  (ensure-have-keys)
  (define common-params
    `((AWSAccessKeyId ,(public-key))
      (SignatureMethod "HmacSHA256")
      (SignatureVersion "2")
      (Timestamp ,(timestamp))
      (Version "2010-08-01")))
  (define all-params (sort (append params common-params)
                           (lambda (a b)
                             (string<? (symbol->string (car a))
                                       (symbol->string (car b))))))
  (define str-to-sign
    (string-append "POST" "\n"
                   (endpoint->host:port (cw-endpoint)) "\n"
                   "/" "\n"
                   (dict->form-urlencoded all-params)))
  (define signature (sha256-encode str-to-sign))
  (define signed-params (append all-params `((Signature ,signature))))
  (log-debug (format "~a" signed-params))
  (define head
    `([Content-Type "application/x-www-form-urlencoded; charset=utf-8"]))
  (define uri (endpoint->uri (cw-endpoint) "/"))
  (define x (post-with-retry uri signed-params head))
  (append (result-proc x)
          ;; If AWS returned a NextToken element in the response XML, we
          ;; need to call again to get more values.
          (match (tags x 'NextToken)
            [(list `(NextToken () ,token))
             (cw (set-next-token params token)
                 result-proc)]
             [else '()])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (secs->str s)
  (seconds->gmt-8601-string 'T/Z s))

(define (str->secs s)
  (cond
   [(false? s) 0]
   [else (gmt-8601-string->seconds s)]))

(define (string->boolean s)
  (cond
   [(string-ci=? "true" s) #t]
   [(string-ci=? "false" s) #f]
   [else
    (define n (string->number s))
    (cond
     [(false? n)
      (error 'string->boolean "cannot convert string \"~a\" to a boolean" s)]
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
  (cond

   [(list? x) (nuke-ws x)]
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
                      #:first-order (lambda (x)
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
  (for/list ([x (in-list (tags x 'member 'Dimensions))])
      (list (string->symbol (first-tag-value x 'Name))
            (first-tag-value x 'Value))))

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
      (lambda (xpr)
        ;; Only get 'member elements that are direct kids of 'Metrics.
        (for/list ([x (in-list (tags (nuke-ws xpr) 'member 'Metrics))])
            (metric (first-tag-value x 'MetricName)
                    (first-tag-value x 'Namespace)
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

(define/contract (put-metric-data namespace data)
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

(define/contract (get-metric-statistics #:metric-name metric-name
                                        #:namespace namespace
                                        #:statistics statistics
                                        #:unit unit
                                        #:start-time start-time
                                        #:end-time end-time
                                        #:period [period 60]
                                        #:dimensions [dimensions '()])
  ((#:metric-name string?
    #:namespace string?
    #:statistics (non-empty-listof statistic/c)
    #:unit unit/c
    #:start-time exact-integer?
    #:end-time exact-integer?)
   (#:period period/c
    #:dimensions dimensions/c)
   . ->* . (listof datum?))
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
      (lambda (xpr)
        (for/list ([x (in-list (tags (nuke-ws xpr) 'member 'Datapoints))])
            (define (get sym f)
              (define v (first-tag-value x sym #f))
              (and v (f v)))
            (datum metric-name
                   #f
                   (get 'Minimum string->number)
                   (get 'Maximum string->number)
                   (get 'Sum string->number)
                   (get 'SampleCount string->number)
                   (get 'Unit string->symbol)
                   (get 'Timestamp str->secs)
                   (first-tag-value x 'Dimensions '()))))))

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
  (for/list ([x (in-list (tags (nuke-ws xpr) 'member 'MetricAlarms))])
      (alarm
       (first-tag-value x 'AlarmName)
       (first-tag-value x 'AlarmDescription)
       (first-tag-value x 'AlarmArn)
       (str->secs (first-tag-value x 'AlarmConfigurationUpdatedTimestamp #f))
       (first-tag-value x 'MetricName)
       (first-tag-value x 'Namespace)
       (string->number (first-tag-value x 'Threshold))
       (first-tag-value x 'ComparisonOperator)
       (first-tag-value x 'AlarmActions)
       (first-tag-value x 'OKActions)
       (first-tag-value x 'InsufficientDataActions)
       (first-tag-value x 'StateValue)
       (first-tag-value x 'StateReason)
       (first-tag-value x 'StateReasonData)
       (str->secs (first-tag-value x 'StateUpdatedTimestamp #f))
       (string->number (first-tag-value x 'Period))
       (string->boolean (first-tag-value x 'ActionsEnabled))
       (string->number (first-tag-value x 'EvaluationPeriods))
       (first-tag-value x 'Statistic)
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
      (lambda (x)
        (for/list ([x (in-list (tags (nuke-ws x) 'member 'AlarmHistoryItems))])
            (alarm-history (str->secs (first-tag-value x 'Timestamp #f))
                           (first-tag-value x 'HistoryItemType)
                           (first-tag-value x 'AlarmName)
                           ;; HistoryData is a bunch of JSON. Punting
                           ;; on parsing that and just returning
                           ;; as-is.
                           (match (tags x 'HistoryData)
                             [`((HistoryData () ,xs ...)) (string-join xs "")]
                             [else #f])
                           (first-tag-value x 'HistorySummary))))))


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

#;
(get-metric-statistics #:metric-name "CPUUtilization"
                       #:namespace "AWS/EC2"
                       #:statistics '(Average Sum Minimum Maximum SampleCount)
                       #:unit 'Percent
                       #:period (* 60 60) ;1 hour
                       #:start-time (- (current-seconds) (* 24 60 60))
                       #:end-time (current-seconds)
                       #:dimensions `((InstanceId "i-cfeb8ba4")) )

(module+ test
  (require rackunit)
  (require "tests/data.rkt")

  (test-case
   "describe-alarms"
   (define xs (describe-alarms))
   (check-equal? (describe-alarms #:alarm-names (map alarm-name xs))
                 xs))

  (test-case
   "put/list/get metric data"
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
   (check-not-equal? (remove-duplicates (append (map datum-min xs-get)
                                                (map datum-max xs-get)
                                                (map datum-sum xs-get)
                                                (map datum-sample-count xs-get)))
                     (list #f)))
  )
