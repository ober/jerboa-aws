#!chezscheme
;;; (jerboa-aws cloudwatch operations) -- CloudWatch operations

(library (jerboa-aws cloudwatch operations)
  (export list-metrics get-metric-statistics put-metric-alarm)
  (import (chezscheme)
          (jerboa-aws cloudwatch api))

  ;; Encode dimensions as member-list parameters
  ;; dimensions: list of (name . value) pairs
  (define (encode-dimensions prefix dimensions)
    (let loop ([ds dimensions] [i 1] [acc '()])
      (if (null? ds)
        (reverse acc)
        (let ([d (car ds)])
          (loop (cdr ds) (+ i 1)
                (cons (cons (string-append prefix ".member." (number->string i) ".Value")
                            (cdr d))
                      (cons (cons (string-append prefix ".member." (number->string i) ".Name")
                                  (car d))
                            acc)))))))

  ;; Encode a list of strings as member-list parameters
  (define (encode-member-list prefix values)
    (let loop ([vs values] [i 1] [acc '()])
      (if (null? vs)
        (reverse acc)
        (loop (cdr vs) (+ i 1)
              (cons (cons (string-append prefix ".member." (number->string i))
                          (car vs))
                    acc)))))

  (define (list-metrics client . args)
    (let ([namespace (kw-ref args 'namespace: #f)]
          [metric-name (kw-ref args 'metric-name: #f)]
          [dimensions (kw-ref args 'dimensions: #f)]
          [next-token (kw-ref args 'next-token: #f)])
      (cw-action/hash client "ListMetrics"
        (append
          (if namespace (list (cons "Namespace" namespace)) '())
          (if metric-name (list (cons "MetricName" metric-name)) '())
          (if dimensions (encode-dimensions "Dimensions" dimensions) '())
          (if next-token (list (cons "NextToken" next-token)) '())))))

  (define (get-metric-statistics client . args)
    (let ([namespace (kw-ref args 'namespace: #f)]
          [metric-name (kw-ref args 'metric-name: #f)]
          [start-time (kw-ref args 'start-time: #f)]
          [end-time (kw-ref args 'end-time: #f)]
          [period (kw-ref args 'period: #f)]
          [statistics (kw-ref args 'statistics: '())]
          [dimensions (kw-ref args 'dimensions: '())]
          [unit (kw-ref args 'unit: #f)])
      (cw-action/hash client "GetMetricStatistics"
        (append
          (if namespace (list (cons "Namespace" namespace)) '())
          (if metric-name (list (cons "MetricName" metric-name)) '())
          (if start-time (list (cons "StartTime" start-time)) '())
          (if end-time (list (cons "EndTime" end-time)) '())
          (if period
            (list (cons "Period"
                    (if (number? period)
                      (number->string period)
                      period)))
            '())
          (encode-member-list "Statistics" statistics)
          (encode-dimensions "Dimensions" dimensions)
          (if unit (list (cons "Unit" unit)) '())))))

  (define (put-metric-alarm client alarm-name . args)
    (let ([namespace (kw-ref args 'namespace: #f)]
          [metric-name (kw-ref args 'metric-name: #f)]
          [statistic (kw-ref args 'statistic: #f)]
          [period (kw-ref args 'period: #f)]
          [evaluation-periods (kw-ref args 'evaluation-periods: #f)]
          [threshold (kw-ref args 'threshold: #f)]
          [comparison-operator (kw-ref args 'comparison-operator: #f)]
          [dimensions (kw-ref args 'dimensions: '())]
          [alarm-actions (kw-ref args 'alarm-actions: '())]
          [alarm-description (kw-ref args 'alarm-description: #f)]
          [unit (kw-ref args 'unit: #f)])
      (cw-action/hash client "PutMetricAlarm"
        (append
          (list (cons "AlarmName" alarm-name))
          (if namespace (list (cons "Namespace" namespace)) '())
          (if metric-name (list (cons "MetricName" metric-name)) '())
          (if statistic (list (cons "Statistic" statistic)) '())
          (if period
            (list (cons "Period"
                    (if (number? period)
                      (number->string period)
                      period)))
            '())
          (if evaluation-periods
            (list (cons "EvaluationPeriods"
                    (if (number? evaluation-periods)
                      (number->string evaluation-periods)
                      evaluation-periods)))
            '())
          (if threshold
            (list (cons "Threshold"
                    (if (number? threshold)
                      (number->string threshold)
                      threshold)))
            '())
          (if comparison-operator
            (list (cons "ComparisonOperator" comparison-operator)) '())
          (encode-dimensions "Dimensions" dimensions)
          (encode-member-list "AlarmActions" alarm-actions)
          (if alarm-description
            (list (cons "AlarmDescription" alarm-description)) '())
          (if unit (list (cons "Unit" unit)) '())))))

  ;; --- Helpers ---
  (define (kw-ref args key default)
    (let loop ([rest args])
      (cond
        [(null? rest) default]
        [(and (pair? (cdr rest)) (eq? (car rest) key))
         (cadr rest)]
        [else (loop (cdr rest))])))

  ) ;; end library
