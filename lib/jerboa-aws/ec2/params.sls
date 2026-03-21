#!chezscheme
;;; (jerboa-aws ec2 params) -- EC2 query parameter encoding

(library (jerboa-aws ec2 params)
  (export ec2-param-list ec2-param-filters ec2-param-tags
          ec2-param-block-device-mappings ec2-param-ip-permissions
          params-merge)
  (import (except (chezscheme) hashtable? cons*))

  ;; Encode a list as EC2 dot-notation: Prefix.1, Prefix.2, ...
  (define (ec2-param-list prefix items)
    (let loop ([items items] [i 1] [result '()])
      (if (null? items)
        (reverse result)
        (loop (cdr items) (+ i 1)
              (cons (cons (string-append prefix "." (number->string i))
                          (car items))
                    result)))))

  ;; Encode filters: Filter.1.Name=..., Filter.1.Value.1=...
  (define (ec2-param-filters filters)
    (let loop ([filters filters] [i 1] [result '()])
      (if (null? filters)
        (reverse result)
        (let* ([f (car filters)]
               [name (car f)]
               [values (cdr f)]
               [prefix (string-append "Filter." (number->string i))]
               [name-param (cons (string-append prefix ".Name") name)]
               [value-params
                (let vloop ([vals values] [j 1] [vres '()])
                  (if (null? vals) (reverse vres)
                    (vloop (cdr vals) (+ j 1)
                      (cons (cons (string-append prefix ".Value."
                                    (number->string j))
                                  (car vals))
                            vres))))])
          (loop (cdr filters) (+ i 1)
                (append (reverse (cons name-param value-params)) result))))))

  ;; Encode tag specifications
  (define (ec2-param-tags resource-type tags)
    (let* ([prefix "TagSpecification.1"]
           [type-param (cons (string-append prefix ".ResourceType") resource-type)]
           [tag-params
            (let loop ([tags tags] [i 1] [result '()])
              (if (null? tags) (reverse result)
                (let ([t (car tags)])
                  (loop (cdr tags) (+ i 1)
                    (cons* (cons (string-append prefix ".Tag."
                                  (number->string i) ".Value")
                                (cdr t))
                           (cons (string-append prefix ".Tag."
                                  (number->string i) ".Key")
                                (car t))
                           result)))))])
      (cons type-param tag-params)))

  ;; Encode block device mappings
  (define (ec2-param-block-device-mappings mappings)
    (let loop ([mappings mappings] [i 1] [result '()])
      (if (null? mappings)
        (reverse result)
        (let* ([m (car mappings)]
               [prefix (string-append "BlockDeviceMapping." (number->string i))]
               [params (list
                 (cons (string-append prefix ".DeviceName")
                       (ht-ref m "DeviceName" "/dev/xvda")))]
               [params (if (ht-ref m "VolumeSize" #f)
                         (cons (cons (string-append prefix ".Ebs.VolumeSize")
                                     (ht-ref m "VolumeSize" ""))
                               params)
                         params)]
               [params (if (ht-ref m "VolumeType" #f)
                         (cons (cons (string-append prefix ".Ebs.VolumeType")
                                     (ht-ref m "VolumeType" ""))
                               params)
                         params)])
          (loop (cdr mappings) (+ i 1)
                (append (reverse params) result))))))

  ;; Encode IP permissions for security groups
  (define (ec2-param-ip-permissions permissions)
    (let loop ([perms permissions] [i 1] [result '()])
      (if (null? perms)
        (reverse result)
        (let* ([p (car perms)]
               [prefix (string-append "IpPermissions." (number->string i))]
               [params (list
                 (cons (string-append prefix ".IpProtocol")
                       (ht-ref p "IpProtocol" "-1")))]
               [params (if (ht-ref p "FromPort" #f)
                         (cons (cons (string-append prefix ".FromPort")
                                     (ht-ref p "FromPort" ""))
                               params)
                         params)]
               [params (if (ht-ref p "ToPort" #f)
                         (cons (cons (string-append prefix ".ToPort")
                                     (ht-ref p "ToPort" ""))
                               params)
                         params)]
               [params (let cidr-loop ([cidrs (ht-ref p "CidrIps" '())]
                                       [j 1] [acc params])
                         (if (null? cidrs) acc
                           (cidr-loop (cdr cidrs) (+ j 1)
                             (cons (cons (string-append prefix ".IpRanges."
                                           (number->string j) ".CidrIp")
                                         (car cidrs))
                                   acc))))])
          (loop (cdr perms) (+ i 1)
                (append (reverse params) result))))))

  ;; Merge parameter lists
  (define (params-merge . param-lists)
    (apply append param-lists))

  ;; Helpers
  (define (ht-ref ht key default)
    (cond
      [(hashtable? ht) (or (hashtable-ref ht key #f) default)]
      [(list? ht) (let ([p (assoc key ht)])
                    (if p (cdr p) default))]
      [else default]))

  (define (hashtable? x)
    (guard (e [#t #f]) (hashtable-size x) #t))

  (define (cons* . args)
    (let loop ([args args])
      (if (null? (cdr args)) (car args)
        (cons (car args) (loop (cdr args))))))

  ) ;; end library
