#!chezscheme
;;; (jerboa-aws cost-optimization-hub operations) -- Cost Optimization Hub operations

(library (jerboa-aws cost-optimization-hub operations)
  (export list-recommendation-summaries)
  (import (chezscheme)
          (jerboa-aws cost-optimization-hub api))

  (define (list-recommendation-summaries client . args)
    (let ([group-by (kw-ref args 'group-by: #f)]
          [max-results (kw-ref args 'max-results: #f)]
          [next-token (kw-ref args 'next-token: #f)]
          [filter (kw-ref args 'filter: #f)])
      (let ([payload (make-hashtable string-hash string=?)])
        (when group-by
          (hashtable-set! payload "groupBy" group-by))
        (when max-results
          (hashtable-set! payload "maxResults" max-results))
        (when next-token
          (hashtable-set! payload "nextToken" next-token))
        (when filter
          (hashtable-set! payload "filter" filter))
        (cost-optimization-hub-action client
          "ListRecommendationSummaries" payload))))

  ;; --- Helpers ---
  (define (kw-ref args key default)
    (let loop ([rest args])
      (cond
        [(null? rest) default]
        [(and (pair? (cdr rest)) (eq? (car rest) key))
         (cadr rest)]
        [else (loop (cdr rest))])))

  ) ;; end library
