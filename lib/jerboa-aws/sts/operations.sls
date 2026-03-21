#!chezscheme
(library (jerboa-aws sts operations)
  (export get-caller-identity get-session-token assume-role
          decode-authorization-message)
  (import (chezscheme)
          (jerboa-aws sts api))

  (define (get-caller-identity client)
    (sts-action/hash client "GetCallerIdentity"))

  (define (get-session-token client . args)
    (let ([duration (kw-ref args 'duration-seconds: #f)])
      (sts-action/hash client "GetSessionToken"
        (if duration
          (list (cons "DurationSeconds" (number->string duration)))
          '()))))

  (define (assume-role client role-arn session-name . args)
    (let ([duration (kw-ref args 'duration-seconds: #f)]
          [policy (kw-ref args 'policy: #f)])
      (sts-action/hash client "AssumeRole"
        (append
          (list (cons "RoleArn" role-arn)
                (cons "RoleSessionName" session-name))
          (if duration
            (list (cons "DurationSeconds" (number->string duration)))
            '())
          (if policy
            (list (cons "Policy" policy))
            '())))))

  (define (decode-authorization-message client encoded-message)
    (sts-action/hash client "DecodeAuthorizationMessage"
      (list (cons "EncodedMessage" encoded-message))))

  (define (kw-ref args key default)
    (let loop ([rest args])
      (cond
        [(null? rest) default]
        [(and (pair? (cdr rest)) (eq? (car rest) key))
         (cadr rest)]
        [else (loop (cdr rest))])))

  ) ;; end library
