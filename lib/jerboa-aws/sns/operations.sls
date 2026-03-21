#!chezscheme
;;; (jerboa-aws sns operations) -- SNS operations

(library (jerboa-aws sns operations)
  (export list-topics create-topic delete-topic
          publish subscribe unsubscribe)
  (import (chezscheme)
          (jerboa-aws sns api))

  (define (list-topics client . args)
    (let ([next-token (kw-ref args 'next-token: #f)])
      (sns-action/hash client "ListTopics"
        (if next-token (list (cons "NextToken" next-token)) '()))))

  (define (create-topic client name . args)
    (let ([attributes (kw-ref args 'attributes: '())])
      (sns-action/hash client "CreateTopic"
        (append
          (list (cons "Name" name))
          (let loop ([as attributes] [i 1] [acc '()])
            (if (null? as)
              (reverse acc)
              (loop (cdr as) (+ i 1)
                    (cons (cons (string-append "Attributes.entry." (number->string i) ".value")
                                (cdar as))
                          (cons (cons (string-append "Attributes.entry." (number->string i) ".key")
                                      (caar as))
                                acc)))))))))

  (define (delete-topic client topic-arn)
    (sns-action client "DeleteTopic"
      (list (cons "TopicArn" topic-arn)))
    (void))

  (define (publish client message . args)
    (let ([topic-arn (kw-ref args 'topic-arn: #f)]
          [target-arn (kw-ref args 'target-arn: #f)]
          [subject (kw-ref args 'subject: #f)]
          [message-structure (kw-ref args 'message-structure: #f)])
      (sns-action/hash client "Publish"
        (append
          (list (cons "Message" message))
          (if topic-arn (list (cons "TopicArn" topic-arn)) '())
          (if target-arn (list (cons "TargetArn" target-arn)) '())
          (if subject (list (cons "Subject" subject)) '())
          (if message-structure
            (list (cons "MessageStructure" message-structure))
            '())))))

  (define (subscribe client topic-arn protocol endpoint)
    (sns-action/hash client "Subscribe"
      (list (cons "TopicArn" topic-arn)
            (cons "Protocol" protocol)
            (cons "Endpoint" endpoint))))

  (define (unsubscribe client subscription-arn)
    (sns-action client "Unsubscribe"
      (list (cons "SubscriptionArn" subscription-arn)))
    (void))

  ;; --- Helpers ---
  (define (kw-ref args key default)
    (let loop ([rest args])
      (cond
        [(null? rest) default]
        [(and (pair? (cdr rest)) (eq? (car rest) key))
         (cadr rest)]
        [else (loop (cdr rest))])))

  ) ;; end library
