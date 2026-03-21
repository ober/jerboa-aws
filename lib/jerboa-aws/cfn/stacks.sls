#!chezscheme
;;; (jerboa-aws cfn stacks) -- CloudFormation Stack operations

(library (jerboa-aws cfn stacks)
  (export list-stacks describe-stacks create-stack update-stack
          delete-stack get-template)
  (import (chezscheme)
          (jerboa-aws cfn api))

  (define (list-stacks client . args)
    (let ([stack-status-filter (kw-ref args 'stack-status-filter: '())]
          [next-token (kw-ref args 'next-token: #f)])
      (cfn-action/hash client "ListStacks"
        (append
          (let loop ([ss stack-status-filter] [i 1] [acc '()])
            (if (null? ss)
              (reverse acc)
              (loop (cdr ss) (+ i 1)
                    (cons (cons (string-append "StackStatusFilter.member."
                                  (number->string i))
                                (car ss))
                          acc))))
          (if next-token (list (cons "NextToken" next-token)) '())))))

  (define (describe-stacks client . args)
    (let ([stack-name (kw-ref args 'stack-name: #f)]
          [next-token (kw-ref args 'next-token: #f)])
      (cfn-action/hash client "DescribeStacks"
        (append
          (if stack-name (list (cons "StackName" stack-name)) '())
          (if next-token (list (cons "NextToken" next-token)) '())))))

  (define (create-stack client stack-name . args)
    (let ([template-body (kw-ref args 'template-body: #f)]
          [template-url (kw-ref args 'template-url: #f)]
          [parameters (kw-ref args 'parameters: '())]
          [capabilities (kw-ref args 'capabilities: '())]
          [on-failure (kw-ref args 'on-failure: #f)]
          [tags (kw-ref args 'tags: '())])
      (cfn-action/hash client "CreateStack"
        (append
          (list (cons "StackName" stack-name))
          (if template-body (list (cons "TemplateBody" template-body)) '())
          (if template-url (list (cons "TemplateURL" template-url)) '())
          (let loop ([ps parameters] [i 1] [acc '()])
            (if (null? ps)
              (reverse acc)
              (loop (cdr ps) (+ i 1)
                    (cons (cons (string-append "Parameters.member."
                                  (number->string i) ".ParameterValue")
                                (cdar ps))
                          (cons (cons (string-append "Parameters.member."
                                        (number->string i) ".ParameterKey")
                                      (caar ps))
                                acc)))))
          (let loop ([cs capabilities] [i 1] [acc '()])
            (if (null? cs)
              (reverse acc)
              (loop (cdr cs) (+ i 1)
                    (cons (cons (string-append "Capabilities.member."
                                  (number->string i))
                                (car cs))
                          acc))))
          (if on-failure (list (cons "OnFailure" on-failure)) '())
          (let loop ([ts tags] [i 1] [acc '()])
            (if (null? ts)
              (reverse acc)
              (loop (cdr ts) (+ i 1)
                    (cons (cons (string-append "Tags.member."
                                  (number->string i) ".Value")
                                (cdar ts))
                          (cons (cons (string-append "Tags.member."
                                        (number->string i) ".Key")
                                      (caar ts))
                                acc)))))))))

  (define (update-stack client stack-name . args)
    (let ([template-body (kw-ref args 'template-body: #f)]
          [template-url (kw-ref args 'template-url: #f)]
          [parameters (kw-ref args 'parameters: '())]
          [capabilities (kw-ref args 'capabilities: '())])
      (cfn-action/hash client "UpdateStack"
        (append
          (list (cons "StackName" stack-name))
          (if template-body (list (cons "TemplateBody" template-body)) '())
          (if template-url (list (cons "TemplateURL" template-url)) '())
          (let loop ([ps parameters] [i 1] [acc '()])
            (if (null? ps)
              (reverse acc)
              (loop (cdr ps) (+ i 1)
                    (cons (cons (string-append "Parameters.member."
                                  (number->string i) ".ParameterValue")
                                (cdar ps))
                          (cons (cons (string-append "Parameters.member."
                                        (number->string i) ".ParameterKey")
                                      (caar ps))
                                acc)))))
          (let loop ([cs capabilities] [i 1] [acc '()])
            (if (null? cs)
              (reverse acc)
              (loop (cdr cs) (+ i 1)
                    (cons (cons (string-append "Capabilities.member."
                                  (number->string i))
                                (car cs))
                          acc))))))))

  (define (delete-stack client stack-name)
    (cfn-action client "DeleteStack"
      (list (cons "StackName" stack-name)))
    (void))

  (define (get-template client stack-name . args)
    (let ([template-stage (kw-ref args 'template-stage: #f)])
      (cfn-action/hash client "GetTemplate"
        (append
          (list (cons "StackName" stack-name))
          (if template-stage
            (list (cons "TemplateStage" template-stage))
            '())))))

  ;; --- Helpers ---
  (define (kw-ref args key default)
    (let loop ([rest args])
      (cond
        [(null? rest) default]
        [(and (pair? (cdr rest)) (eq? (car rest) key))
         (cadr rest)]
        [else (loop (cdr rest))])))

  ) ;; end library
