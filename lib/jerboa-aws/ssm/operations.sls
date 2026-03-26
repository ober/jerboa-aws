#!chezscheme
;;; (jerboa-aws ssm operations) -- AWS Systems Manager operations

(library (jerboa-aws ssm operations)
  (export put-parameter get-parameter get-parameters
          delete-parameter describe-instance-information
          send-command get-command-invocation)
  (import (chezscheme)
          (jerboa-aws ssm api))

  (define (put-parameter client name value . args)
    (let ([type (kw-ref args 'type: "String")]
          [overwrite (kw-ref args 'overwrite: #f)]
          [description (kw-ref args 'description: #f)]
          [tier (kw-ref args 'tier: #f)])
      (let ([payload (make-hashtable string-hash string=?)])
        (hashtable-set! payload "Name" name)
        (hashtable-set! payload "Value" value)
        (hashtable-set! payload "Type" type)
        (when overwrite (hashtable-set! payload "Overwrite" overwrite))
        (when description (hashtable-set! payload "Description" description))
        (when tier (hashtable-set! payload "Tier" tier))
        (ssm-action client "PutParameter" payload))))

  (define (get-parameter client name . args)
    (let ([with-decryption (kw-ref args 'with-decryption: #f)])
      (let ([payload (make-hashtable string-hash string=?)])
        (hashtable-set! payload "Name" name)
        (when with-decryption
          (hashtable-set! payload "WithDecryption" with-decryption))
        (ssm-action client "GetParameter" payload))))

  (define (get-parameters client names . args)
    (let ([with-decryption (kw-ref args 'with-decryption: #f)])
      (let ([payload (make-hashtable string-hash string=?)])
        (hashtable-set! payload "Names" (list->vector names))
        (when with-decryption
          (hashtable-set! payload "WithDecryption" with-decryption))
        (ssm-action client "GetParameters" payload))))

  (define (delete-parameter client name)
    (let ([payload (make-hashtable string-hash string=?)])
      (hashtable-set! payload "Name" name)
      (ssm-action client "DeleteParameter" payload))
    (void))

  (define (describe-instance-information client . args)
    (let ([filters (kw-ref args 'filters: #f)]
          [max-results (kw-ref args 'max-results: #f)]
          [next-token (kw-ref args 'next-token: #f)])
      (let ([payload (make-hashtable string-hash string=?)])
        (when filters
          (hashtable-set! payload "Filters" (list->vector filters)))
        (when max-results
          (hashtable-set! payload "MaxResults" max-results))
        (when next-token
          (hashtable-set! payload "NextToken" next-token))
        (ssm-action client "DescribeInstanceInformation" payload))))

  (define (send-command client instance-ids command . args)
    (let ([document-name (kw-ref args 'document-name: "AWS-RunShellScript")]
          [comment (kw-ref args 'comment: #f)])
      (let ([payload (make-hashtable string-hash string=?)])
        (hashtable-set! payload "DocumentName" document-name)
        (hashtable-set! payload "InstanceIds" (list->vector instance-ids))
        (let ([params (make-hashtable string-hash string=?)])
          (hashtable-set! params "commands" (vector command))
          (hashtable-set! payload "Parameters" params))
        (when comment (hashtable-set! payload "Comment" comment))
        (ssm-action client "SendCommand" payload))))

  (define (get-command-invocation client command-id instance-id)
    (let ([payload (make-hashtable string-hash string=?)])
      (hashtable-set! payload "CommandId" command-id)
      (hashtable-set! payload "InstanceId" instance-id)
      (ssm-action client "GetCommandInvocation" payload)))

  ;; --- Helpers ---
  (define (kw-ref args key default)
    (let loop ([rest args])
      (cond
        [(null? rest) default]
        [(and (pair? (cdr rest)) (eq? (car rest) key))
         (cadr rest)]
        [else (loop (cdr rest))])))

  ) ;; end library
