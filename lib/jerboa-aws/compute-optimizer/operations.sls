#!chezscheme
;;; (jerboa-aws compute-optimizer operations) -- Compute Optimizer operations

(library (jerboa-aws compute-optimizer operations)
  (export get-auto-scaling-group-recommendations)
  (import (chezscheme)
          (jerboa-aws compute-optimizer api))

  (define (get-auto-scaling-group-recommendations client . args)
    (let ([auto-scaling-group-arns (kw-ref args 'auto-scaling-group-arns: #f)]
          [max-results (kw-ref args 'max-results: #f)]
          [next-token (kw-ref args 'next-token: #f)]
          [filters (kw-ref args 'filters: #f)])
      (let ([payload (make-hashtable string-hash string=?)])
        (when auto-scaling-group-arns
          (hashtable-set! payload "autoScalingGroupArns"
            (list->vector auto-scaling-group-arns)))
        (when max-results
          (hashtable-set! payload "maxResults" max-results))
        (when next-token
          (hashtable-set! payload "nextToken" next-token))
        (when filters
          (hashtable-set! payload "filters" (list->vector filters)))
        (compute-optimizer-action client
          "GetAutoScalingGroupRecommendations" payload))))

  ;; --- Helpers ---
  (define (kw-ref args key default)
    (let loop ([rest args])
      (cond
        [(null? rest) default]
        [(and (pair? (cdr rest)) (eq? (car rest) key))
         (cadr rest)]
        [else (loop (cdr rest))])))

  ) ;; end library
