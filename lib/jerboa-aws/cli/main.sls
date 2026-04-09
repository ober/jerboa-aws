#!chezscheme
;;; (jerboa-aws cli main) -- CLI entry point with subcommand dispatch

(library (jerboa-aws cli main)
  (export main)
  (import (chezscheme)
          (jerboa-aws json)
          (jerboa-aws cli format)
          ;; EC2
          (jerboa-aws ec2 api)
          (jerboa-aws ec2 instances)
          (jerboa-aws ec2 security-groups)
          (jerboa-aws ec2 vpcs)
          (jerboa-aws ec2 subnets)
          (jerboa-aws ec2 volumes)
          (jerboa-aws ec2 snapshots)
          (jerboa-aws ec2 addresses)
          (jerboa-aws ec2 network-interfaces)
          (jerboa-aws ec2 key-pairs)
          ;; S3
          (except (jerboa-aws s3 api) make-s3-client)
          (jerboa-aws s3 buckets)
          (jerboa-aws s3 objects)
          ;; STS
          (jerboa-aws sts api)
          (jerboa-aws sts operations)
          ;; IAM
          (jerboa-aws iam api)
          (jerboa-aws iam users)
          (jerboa-aws iam groups)
          (jerboa-aws iam roles)
          (jerboa-aws iam policies)
          (jerboa-aws iam access-keys)
          ;; SSM
          (jerboa-aws ssm api)
          (jerboa-aws ssm operations))

  ;; ---- Keyword argument helpers ----

  (define (kw-ref args key default)
    (let loop ((rest args))
      (cond
        ((null? rest) default)
        ((null? (cdr rest)) default)
        ((eq? (car rest) key) (cadr rest))
        (else (loop (cddr rest))))))

  ;; ---- Argument parsing ----
  ;; Parse CLI args into: (values subcommand action rest-args global-opts)
  ;; Global options: --profile, --region, --output

  (define (parse-global-opts args)
    ;; Returns (values remaining-args profile region output-format)
    (let loop ((rest args) (acc '()) (profile #f) (region #f) (output "json"))
      (cond
        ((null? rest)
         (values (reverse acc) profile region output))
        ((and (string=? (car rest) "--profile") (pair? (cdr rest)))
         (loop (cddr rest) acc (cadr rest) region output))
        ((and (string=? (car rest) "-p") (pair? (cdr rest)))
         (loop (cddr rest) acc (cadr rest) region output))
        ((and (string=? (car rest) "--region") (pair? (cdr rest)))
         (loop (cddr rest) acc profile (cadr rest) output))
        ((and (string=? (car rest) "-r") (pair? (cdr rest)))
         (loop (cddr rest) acc profile (cadr rest) output))
        ((and (string=? (car rest) "--output") (pair? (cdr rest)))
         (loop (cddr rest) acc profile region (cadr rest)))
        ((and (string=? (car rest) "-o") (pair? (cdr rest)))
         (loop (cddr rest) acc profile region (cadr rest)))
        (else
         (loop (cdr rest) (cons (car rest) acc) profile region output)))))

  ;; Extract a named option from positional args: --name value
  (define (get-opt args name)
    (let loop ((rest args))
      (cond
        ((null? rest) #f)
        ((null? (cdr rest)) #f)
        ((string=? (car rest) name) (cadr rest))
        (else (loop (cdr rest))))))

  ;; Extract a flag from positional args: --flag
  (define (get-flag args name)
    (let loop ((rest args))
      (cond
        ((null? rest) #f)
        ((string=? (car rest) name) #t)
        (else (loop (cdr rest))))))

  ;; Parse comma-separated string into list
  (define (parse-csv str)
    (if (or (not str) (string=? str ""))
      '()
      (string-split str #\,)))

  ;; Parse "name=val1,val2;name2=val3" into filter list
  (define (parse-filters str)
    (if (or (not str) (string=? str ""))
      '()
      (map (lambda (f)
             (let ((parts (string-split f #\=)))
               (if (>= (length parts) 2)
                 (cons (car parts) (string-split (cadr parts) #\,))
                 (cons (car parts) '()))))
           (string-split str #\;))))

  ;; Split string by character
  (define (string-split str ch)
    (let loop ((i 0) (start 0) (acc '()))
      (cond
        ((= i (string-length str))
         (reverse (cons (substring str start i) acc)))
        ((char=? (string-ref str i) ch)
         (loop (+ i 1) (+ i 1) (cons (substring str start i) acc)))
        (else
         (loop (+ i 1) start acc)))))

  ;; ---- Client constructors ----

  (define (make-ec2-client profile region)
    (apply EC2Client
      (append
        (if profile (list 'profile: profile) '())
        (if region  (list 'region: region)  '()))))

  (define (make-s3-client profile region)
    (apply S3Client
      (append
        (if profile (list 'profile: profile) '())
        (if region  (list 'region: region)  '()))))

  (define (make-sts-client profile region)
    (apply STSClient
      (append
        (if profile (list 'profile: profile) '())
        (if region  (list 'region: region)  '()))))

  (define (make-iam-client profile region)
    (apply IAMClient
      (append
        (if profile (list 'profile: profile) '())
        (if region  (list 'region: region)  '()))))

  (define (make-ssm-client profile region)
    (apply SSMClient
      (append
        (if profile (list 'profile: profile) '())
        (if region  (list 'region: region)  '()))))

  ;; ---- Output helper ----

  (define (output-result result output-fmt)
    (format-output output-fmt result))

  ;; ---- Usage ----

  (define (print-usage)
    (display "jerboa-aws -- AWS command-line interface

Usage: jerboa-aws [--profile NAME] [--region REGION] [--output FORMAT] <service> <action> [options]

Services:
  ec2          EC2 instances, VPCs, security groups, etc.
  s3           S3 high-level (ls, cp, mv, rm, mb, rb)
  s3api        S3 low-level API (list-buckets, get-object, etc.)
  sts          STS identity and session operations
  iam          IAM users, groups, roles, policies
  ssm          Systems Manager parameters and commands

Global options:
  --profile, -p NAME     AWS profile name
  --region, -r REGION    AWS region
  --output, -o FORMAT    Output format: json, text (default: json)

Run 'jerboa-aws <service> help' for service-specific commands.
")
    (exit 0))

  ;; ---- EC2 subcommands ----

  (define (ec2-dispatch action args profile region output-fmt)
    (let ((client (make-ec2-client profile region)))
      (cond
        ((string=? action "describe-instances")
         (let ((ids (parse-csv (or (get-opt args "--instance-ids") "")))
               (filters (parse-filters (or (get-opt args "--filters") ""))))
           (output-result
             (describe-instances client 'instance-ids: ids 'filters: filters)
             output-fmt)))
        ((string=? action "run-instances")
         (let ((image-id (or (get-opt args "--image-id")
                             (error 'ec2 "run-instances requires --image-id")))
               (instance-type (or (get-opt args "--instance-type") "t2.micro"))
               (count (or (get-opt args "--count") "1"))
               (key-name (get-opt args "--key-name"))
               (sg-ids (parse-csv (or (get-opt args "--security-group-ids") "")))
               (subnet-id (get-opt args "--subnet-id")))
           (output-result
             (run-instances client image-id instance-type
               'min-count: count 'max-count: count
               'key-name: key-name
               'security-group-ids: sg-ids
               'subnet-id: subnet-id)
             output-fmt)))
        ((string=? action "start-instances")
         (let ((ids (parse-csv (or (get-opt args "--instance-ids")
                                   (error 'ec2 "start-instances requires --instance-ids")))))
           (output-result (start-instances client ids) output-fmt)))
        ((string=? action "stop-instances")
         (let ((ids (parse-csv (or (get-opt args "--instance-ids")
                                   (error 'ec2 "stop-instances requires --instance-ids"))))
               (force? (get-flag args "--force")))
           (output-result (stop-instances client ids 'force: force?) output-fmt)))
        ((string=? action "terminate-instances")
         (let ((ids (parse-csv (or (get-opt args "--instance-ids")
                                   (error 'ec2 "terminate-instances requires --instance-ids")))))
           (output-result (terminate-instances client ids) output-fmt)))
        ((string=? action "reboot-instances")
         (let ((ids (parse-csv (or (get-opt args "--instance-ids")
                                   (error 'ec2 "reboot-instances requires --instance-ids")))))
           (output-result (reboot-instances client ids) output-fmt)))
        ((string=? action "describe-instance-status")
         (let ((ids (parse-csv (or (get-opt args "--instance-ids") ""))))
           (output-result (describe-instance-status client 'instance-ids: ids) output-fmt)))
        ((string=? action "describe-instance-types")
         (output-result (describe-instance-types client) output-fmt))
        ((string=? action "describe-security-groups")
         (let ((ids (parse-csv (or (get-opt args "--group-ids") "")))
               (filters (parse-filters (or (get-opt args "--filters") ""))))
           (output-result
             (describe-security-groups client 'group-ids: ids 'filters: filters)
             output-fmt)))
        ((string=? action "create-security-group")
         (let* ((name (or (get-opt args "--group-name")
                          (error 'ec2 "create-security-group requires --group-name")))
                (desc (or (get-opt args "--description") name))
                (vpc-id (get-opt args "--vpc-id")))
           (output-result
             (create-security-group client name desc 'vpc-id: vpc-id)
             output-fmt)))
        ((string=? action "delete-security-group")
         (let ((id (or (get-opt args "--group-id")
                       (error 'ec2 "delete-security-group requires --group-id"))))
           (output-result (delete-security-group client id) output-fmt)))
        ((string=? action "describe-vpcs")
         (let ((ids (parse-csv (or (get-opt args "--vpc-ids") "")))
               (filters (parse-filters (or (get-opt args "--filters") ""))))
           (output-result
             (describe-vpcs client 'vpc-ids: ids 'filters: filters)
             output-fmt)))
        ((string=? action "create-vpc")
         (let ((cidr (or (get-opt args "--cidr-block")
                         (error 'ec2 "create-vpc requires --cidr-block"))))
           (output-result (create-vpc client cidr) output-fmt)))
        ((string=? action "delete-vpc")
         (let ((id (or (get-opt args "--vpc-id")
                       (error 'ec2 "delete-vpc requires --vpc-id"))))
           (output-result (delete-vpc client id) output-fmt)))
        ((string=? action "describe-subnets")
         (let ((ids (parse-csv (or (get-opt args "--subnet-ids") "")))
               (filters (parse-filters (or (get-opt args "--filters") ""))))
           (output-result
             (describe-subnets client 'subnet-ids: ids 'filters: filters)
             output-fmt)))
        ((string=? action "create-subnet")
         (let ((vpc-id (or (get-opt args "--vpc-id")
                           (error 'ec2 "create-subnet requires --vpc-id")))
               (cidr (or (get-opt args "--cidr-block")
                         (error 'ec2 "create-subnet requires --cidr-block"))))
           (output-result (create-subnet client vpc-id cidr) output-fmt)))
        ((string=? action "delete-subnet")
         (let ((id (or (get-opt args "--subnet-id")
                       (error 'ec2 "delete-subnet requires --subnet-id"))))
           (output-result (delete-subnet client id) output-fmt)))
        ((string=? action "describe-volumes")
         (let ((ids (parse-csv (or (get-opt args "--volume-ids") "")))
               (filters (parse-filters (or (get-opt args "--filters") ""))))
           (output-result
             (describe-volumes client 'volume-ids: ids 'filters: filters)
             output-fmt)))
        ((string=? action "describe-snapshots")
         (let ((ids (parse-csv (or (get-opt args "--snapshot-ids") "")))
               (filters (parse-filters (or (get-opt args "--filters") ""))))
           (output-result
             (describe-snapshots client 'snapshot-ids: ids 'filters: filters)
             output-fmt)))
        ((string=? action "describe-addresses")
         (output-result (describe-addresses client) output-fmt))
        ((string=? action "describe-key-pairs")
         (output-result (describe-key-pairs client) output-fmt))
        ((string=? action "describe-network-interfaces")
         (let ((ids (parse-csv (or (get-opt args "--network-interface-ids") "")))
               (filters (parse-filters (or (get-opt args "--filters") ""))))
           (output-result
             (describe-network-interfaces client 'network-interface-ids: ids 'filters: filters)
             output-fmt)))
        ((or (string=? action "help") (string=? action "--help"))
         (display "jerboa-aws ec2 commands:
  describe-instances       [--instance-ids IDS] [--filters FILTERS]
  run-instances            --image-id AMI [--instance-type TYPE] [--count N]
  start-instances          --instance-ids IDS
  stop-instances           --instance-ids IDS [--force]
  terminate-instances      --instance-ids IDS
  reboot-instances         --instance-ids IDS
  describe-instance-status [--instance-ids IDS]
  describe-instance-types
  describe-security-groups [--group-ids IDS] [--filters FILTERS]
  create-security-group    --group-name NAME [--description DESC] [--vpc-id VPC]
  delete-security-group    --group-id ID
  describe-vpcs            [--vpc-ids IDS] [--filters FILTERS]
  create-vpc               --cidr-block CIDR
  delete-vpc               --vpc-id ID
  describe-subnets         [--subnet-ids IDS] [--filters FILTERS]
  create-subnet            --vpc-id VPC --cidr-block CIDR
  delete-subnet            --subnet-id ID
  describe-volumes         [--volume-ids IDS] [--filters FILTERS]
  describe-snapshots       [--snapshot-ids IDS] [--filters FILTERS]
  describe-addresses
  describe-key-pairs
  describe-network-interfaces [--network-interface-ids IDS] [--filters FILTERS]
")
         (exit 0))
        (else
         (display (format "Unknown ec2 action: ~a\nRun 'jerboa-aws ec2 help' for available commands.\n" action))
         (exit 1)))))

  ;; ---- S3 URI helpers ----

  (define (s3-uri? str)
    (and (string? str)
         (>= (string-length str) 5)
         (string=? (substring str 0 5) "s3://")))

  ;; Parse "s3://bucket" → (cons "bucket" #f)
  ;; Parse "s3://bucket/" → (cons "bucket" "")
  ;; Parse "s3://bucket/key/path" → (cons "bucket" "key/path")
  (define (parse-s3-uri uri)
    (if (s3-uri? uri)
      (let* ((rest (substring uri 5 (string-length uri)))
             (slash (let loop ((i 0))
                      (cond
                        ((= i (string-length rest)) #f)
                        ((char=? (string-ref rest i) #\/) i)
                        (else (loop (+ i 1)))))))
        (if slash
          (cons (substring rest 0 slash)
                (substring rest (+ slash 1) (string-length rest)))
          (cons rest #f)))
      (error 's3 "expected s3:// URI" uri)))

  ;; ---- S3 high-level subcommands (aws-cli compatible) ----

  (define (s3-dispatch action args profile region output-fmt)
    (let ((client (make-s3-client profile region)))
      (cond
        ;; ls [s3://BUCKET[/PREFIX]]  -- list buckets (no arg) or objects
        ((string=? action "ls")
         (if (null? args)
           (output-result (list-buckets client) output-fmt)
           (let* ((uri (car args))
                  (parsed (parse-s3-uri uri))
                  (bucket (car parsed))
                  (prefix (cdr parsed)))
             (output-result
               (apply list-objects-v2 client bucket
                 (if (and prefix (> (string-length prefix) 0))
                   (list 'prefix: prefix)
                   '()))
               output-fmt))))
        ;; cp <SRC> <DST>  -- copy: s3↔local or s3↔s3
        ((string=? action "cp")
         (when (< (length args) 2)
           (display "usage: aws s3 cp <src> <dst> [--content-type TYPE]\n")
           (exit 1))
         (let ((src (car args))
               (dst (cadr args))
               (content-type (or (get-opt (cddr args) "--content-type")
                                 "application/octet-stream")))
           (cond
             ;; s3://src → s3://dst  (server-side copy)
             ((and (s3-uri? src) (s3-uri? dst))
              (let* ((sp (parse-s3-uri src))
                     (dp (parse-s3-uri dst)))
                (copy-object client (car dp) (cdr dp)
                  (string-append "/" (car sp) "/" (cdr sp)))
                (display (format "copy: ~a to ~a\n" src dst))))
             ;; s3://src → local  (download)
             ((s3-uri? src)
              (let* ((p (parse-s3-uri src))
                     (data (get-object client (car p) (cdr p))))
                (if (string=? dst "-")
                  (display data)
                  (begin
                    (call-with-output-file dst
                      (lambda (port) (display data port))
                      'replace)
                    (display (format "download: ~a to ~a\n" src dst))))))
             ;; local → s3://dst  (upload)
             ((s3-uri? dst)
              (let* ((p (parse-s3-uri dst))
                     (data (call-with-input-file src
                             (lambda (port) (get-string-all port)))))
                (put-object client (car p) (cdr p) data
                  'content-type: content-type)
                (display (format "upload: ~a to ~a\n" src dst))))
             (else
              (error 's3 "cp: at least one argument must be an s3:// URI")))))
        ;; mv <SRC> <DST>  -- move: s3↔local or s3↔s3
        ((string=? action "mv")
         (when (< (length args) 2)
           (display "usage: aws s3 mv <src> <dst>\n")
           (exit 1))
         (let ((src (car args))
               (dst (cadr args)))
           (cond
             ;; s3://src → s3://dst  (copy + delete)
             ((and (s3-uri? src) (s3-uri? dst))
              (let* ((sp (parse-s3-uri src))
                     (dp (parse-s3-uri dst)))
                (copy-object client (car dp) (cdr dp)
                  (string-append "/" (car sp) "/" (cdr sp)))
                (delete-object client (car sp) (cdr sp))
                (display (format "move: ~a to ~a\n" src dst))))
             ;; s3://src → local  (download + delete)
             ((s3-uri? src)
              (let* ((p (parse-s3-uri src))
                     (data (get-object client (car p) (cdr p))))
                (if (string=? dst "-")
                  (display data)
                  (call-with-output-file dst
                    (lambda (port) (display data port))
                    'replace))
                (delete-object client (car p) (cdr p))
                (display (format "move: ~a to ~a\n" src dst))))
             (else
              (error 's3 "mv: at least one argument must be an s3:// URI")))))
        ;; rm <s3://BUCKET/KEY>  -- delete object
        ((string=? action "rm")
         (when (null? args)
           (display "usage: aws s3 rm <s3://bucket/key>\n")
           (exit 1))
         (let* ((p (parse-s3-uri (car args))))
           (delete-object client (car p) (cdr p))
           (display (format "delete: ~a\n" (car args)))))
        ;; mb <s3://BUCKET>  -- make bucket
        ((string=? action "mb")
         (when (null? args)
           (display "usage: aws s3 mb <s3://bucket>\n")
           (exit 1))
         (let* ((p (parse-s3-uri (car args))))
           (create-bucket client (car p)
             'region: (or region "us-east-1"))
           (display (format "make_bucket: ~a\n" (car p)))))
        ;; rb <s3://BUCKET>  -- remove bucket
        ((string=? action "rb")
         (when (null? args)
           (display "usage: aws s3 rb <s3://bucket>\n")
           (exit 1))
         (let* ((p (parse-s3-uri (car args))))
           (delete-bucket client (car p))
           (display (format "remove_bucket: ~a\n" (car p)))))
        ((or (string=? action "help") (string=? action "--help"))
         (display "jerboa-aws s3 high-level commands (aws-cli compatible):
  ls [s3://BUCKET[/PREFIX]]              list buckets or objects
  cp <SRC> <DST> [--content-type TYPE]   copy: local↔s3 or s3↔s3
  mv <SRC> <DST>                         move: local↔s3 or s3↔s3
  rm <s3://BUCKET/KEY>                   delete object
  mb <s3://BUCKET>                       make bucket
  rb <s3://BUCKET>                       remove bucket

For low-level API commands use 's3api':
  jerboa-aws s3api list-buckets
  jerboa-aws s3api get-object --bucket NAME --key KEY
  jerboa-aws s3api put-object --bucket NAME --key KEY --body CONTENT
  etc.
")
         (exit 0))
        (else
         (display (format "Unknown s3 action: ~a\nRun 'jerboa-aws s3 help' for available commands.\n" action))
         (exit 1)))))

  ;; ---- S3 low-level API subcommands (s3api, verbose names) ----

  (define (s3api-dispatch action args profile region output-fmt)
    (let ((client (make-s3-client profile region)))
      (cond
        ((string=? action "list-buckets")
         (output-result (list-buckets client) output-fmt))
        ((string=? action "create-bucket")
         (let ((bucket (or (get-opt args "--bucket")
                           (error 's3api "create-bucket requires --bucket"))))
           (output-result (create-bucket client bucket) output-fmt)))
        ((string=? action "delete-bucket")
         (let ((bucket (or (get-opt args "--bucket")
                           (error 's3api "delete-bucket requires --bucket"))))
           (output-result (delete-bucket client bucket) output-fmt)))
        ((string=? action "head-bucket")
         (let ((bucket (or (get-opt args "--bucket")
                           (error 's3api "head-bucket requires --bucket"))))
           (output-result (head-bucket client bucket) output-fmt)))
        ((string=? action "get-bucket-location")
         (let ((bucket (or (get-opt args "--bucket")
                           (error 's3api "get-bucket-location requires --bucket"))))
           (output-result (get-bucket-location client bucket) output-fmt)))
        ((string=? action "list-objects-v2")
         (let ((bucket (or (get-opt args "--bucket")
                           (error 's3api "list-objects-v2 requires --bucket")))
               (prefix (get-opt args "--prefix")))
           (output-result
             (apply list-objects-v2 client bucket
               (if prefix (list 'prefix: prefix) '()))
             output-fmt)))
        ((string=? action "get-object")
         (let ((bucket (or (get-opt args "--bucket")
                           (error 's3api "get-object requires --bucket")))
               (key (or (get-opt args "--key")
                        (error 's3api "get-object requires --key"))))
           (output-result (get-object client bucket key) output-fmt)))
        ((string=? action "put-object")
         (let ((bucket (or (get-opt args "--bucket")
                           (error 's3api "put-object requires --bucket")))
               (key (or (get-opt args "--key")
                        (error 's3api "put-object requires --key")))
               (body (or (get-opt args "--body")
                         (error 's3api "put-object requires --body"))))
           (output-result (put-object client bucket key body) output-fmt)))
        ((string=? action "delete-object")
         (let ((bucket (or (get-opt args "--bucket")
                           (error 's3api "delete-object requires --bucket")))
               (key (or (get-opt args "--key")
                        (error 's3api "delete-object requires --key"))))
           (output-result (delete-object client bucket key) output-fmt)))
        ((string=? action "head-object")
         (let ((bucket (or (get-opt args "--bucket")
                           (error 's3api "head-object requires --bucket")))
               (key (or (get-opt args "--key")
                        (error 's3api "head-object requires --key"))))
           (output-result (head-object client bucket key) output-fmt)))
        ((string=? action "copy-object")
         (let ((src-bucket (or (get-opt args "--source-bucket")
                               (error 's3api "copy-object requires --source-bucket")))
               (src-key (or (get-opt args "--source-key")
                            (error 's3api "copy-object requires --source-key")))
               (dst-bucket (or (get-opt args "--bucket")
                               (error 's3api "copy-object requires --bucket")))
               (dst-key (or (get-opt args "--key")
                            (error 's3api "copy-object requires --key"))))
           (output-result
             (copy-object client dst-bucket dst-key
               (string-append "/" src-bucket "/" src-key))
             output-fmt)))
        ((or (string=? action "help") (string=? action "--help"))
         (display "jerboa-aws s3api commands (low-level API):
  list-buckets
  create-bucket          --bucket NAME
  delete-bucket          --bucket NAME
  head-bucket            --bucket NAME
  get-bucket-location    --bucket NAME
  list-objects-v2        --bucket NAME [--prefix PREFIX]
  get-object             --bucket NAME --key KEY
  put-object             --bucket NAME --key KEY --body CONTENT
  delete-object          --bucket NAME --key KEY
  head-object            --bucket NAME --key KEY
  copy-object            --source-bucket SRC --source-key SKEY --bucket DST --key DKEY
")
         (exit 0))
        (else
         (display (format "Unknown s3api action: ~a\nRun 'jerboa-aws s3api help' for available commands.\n" action))
         (exit 1)))))

  ;; ---- STS subcommands ----

  (define (sts-dispatch action args profile region output-fmt)
    (let ((client (make-sts-client profile region)))
      (cond
        ((string=? action "get-caller-identity")
         (output-result (get-caller-identity client) output-fmt))
        ((string=? action "get-session-token")
         (let ((duration (get-opt args "--duration-seconds")))
           (output-result
             (apply get-session-token client
               (if duration (list 'duration-seconds: (string->number duration)) '()))
             output-fmt)))
        ((string=? action "assume-role")
         (let ((role-arn (or (get-opt args "--role-arn")
                             (error 'sts "assume-role requires --role-arn")))
               (session-name (or (get-opt args "--session-name")
                                 (error 'sts "assume-role requires --session-name")))
               (duration (get-opt args "--duration-seconds")))
           (output-result
             (apply assume-role client role-arn session-name
               (if duration (list 'duration-seconds: (string->number duration)) '()))
             output-fmt)))
        ((or (string=? action "help") (string=? action "--help"))
         (display "jerboa-aws sts commands:
  get-caller-identity
  get-session-token    [--duration-seconds N]
  assume-role          --role-arn ARN --session-name NAME [--duration-seconds N]
")
         (exit 0))
        (else
         (display (format "Unknown sts action: ~a\nRun 'jerboa-aws sts help' for available commands.\n" action))
         (exit 1)))))

  ;; ---- IAM subcommands ----

  (define (iam-dispatch action args profile region output-fmt)
    (let ((client (make-iam-client profile region)))
      (cond
        ;; Users
        ((string=? action "list-users")
         (output-result (list-users client) output-fmt))
        ((string=? action "get-user")
         (let ((name (or (get-opt args "--user-name")
                         (error 'iam "get-user requires --user-name"))))
           (output-result (get-user client 'user-name: name) output-fmt)))
        ((string=? action "create-user")
         (let ((name (or (get-opt args "--user-name")
                         (error 'iam "create-user requires --user-name"))))
           (output-result (create-user client name) output-fmt)))
        ((string=? action "delete-user")
         (let ((name (or (get-opt args "--user-name")
                         (error 'iam "delete-user requires --user-name"))))
           (output-result (delete-user client name) output-fmt)))
        ;; Groups
        ((string=? action "list-groups")
         (output-result (list-groups client) output-fmt))
        ((string=? action "get-group")
         (let ((name (or (get-opt args "--group-name")
                         (error 'iam "get-group requires --group-name"))))
           (output-result (get-group client name) output-fmt)))
        ((string=? action "create-group")
         (let ((name (or (get-opt args "--group-name")
                         (error 'iam "create-group requires --group-name"))))
           (output-result (create-group client name) output-fmt)))
        ((string=? action "delete-group")
         (let ((name (or (get-opt args "--group-name")
                         (error 'iam "delete-group requires --group-name"))))
           (output-result (delete-group client name) output-fmt)))
        ;; Roles
        ((string=? action "list-roles")
         (output-result (list-roles client) output-fmt))
        ((string=? action "get-role")
         (let ((name (or (get-opt args "--role-name")
                         (error 'iam "get-role requires --role-name"))))
           (output-result (get-role client name) output-fmt)))
        ((string=? action "create-role")
         (let ((name (or (get-opt args "--role-name")
                         (error 'iam "create-role requires --role-name")))
               (policy-doc (or (get-opt args "--assume-role-policy-document")
                               (error 'iam "create-role requires --assume-role-policy-document"))))
           (output-result (create-role client name policy-doc) output-fmt)))
        ((string=? action "delete-role")
         (let ((name (or (get-opt args "--role-name")
                         (error 'iam "delete-role requires --role-name"))))
           (output-result (delete-role client name) output-fmt)))
        ;; Policies
        ((string=? action "list-policies")
         (output-result (list-policies client) output-fmt))
        ((string=? action "get-policy")
         (let ((arn (or (get-opt args "--policy-arn")
                        (error 'iam "get-policy requires --policy-arn"))))
           (output-result (get-policy client arn) output-fmt)))
        ;; Access Keys
        ((string=? action "list-access-keys")
         (let ((name (get-opt args "--user-name")))
           (output-result
             (apply list-access-keys client
               (if name (list 'user-name: name) '()))
             output-fmt)))
        ((string=? action "create-access-key")
         (let ((name (get-opt args "--user-name")))
           (output-result
             (apply create-access-key client
               (if name (list 'user-name: name) '()))
             output-fmt)))
        ((string=? action "delete-access-key")
         (let ((key-id (or (get-opt args "--access-key-id")
                           (error 'iam "delete-access-key requires --access-key-id")))
               (name (get-opt args "--user-name")))
           (output-result
             (apply delete-access-key client key-id
               (if name (list 'user-name: name) '()))
             output-fmt)))
        ((or (string=? action "help") (string=? action "--help"))
         (display "jerboa-aws iam commands:
  list-users
  get-user             --user-name NAME
  create-user          --user-name NAME
  delete-user          --user-name NAME
  list-groups
  get-group            --group-name NAME
  create-group         --group-name NAME
  delete-group         --group-name NAME
  list-roles
  get-role             --role-name NAME
  create-role          --role-name NAME --assume-role-policy-document JSON
  delete-role          --role-name NAME
  list-policies
  get-policy           --policy-arn ARN
  list-access-keys     [--user-name NAME]
  create-access-key    [--user-name NAME]
  delete-access-key    --access-key-id ID [--user-name NAME]
")
         (exit 0))
        (else
         (display (format "Unknown iam action: ~a\nRun 'jerboa-aws iam help' for available commands.\n" action))
         (exit 1)))))

  ;; ---- SSM subcommands ----

  (define (ssm-dispatch action args profile region output-fmt)
    (let ((client (make-ssm-client profile region)))
      (cond
        ((string=? action "put-parameter")
         (let ((name (or (get-opt args "--name")
                         (error 'ssm "put-parameter requires --name")))
               (value (or (get-opt args "--value")
                          (error 'ssm "put-parameter requires --value")))
               (type (or (get-opt args "--type") "String"))
               (overwrite (get-flag args "--overwrite")))
           (output-result
             (put-parameter client name value 'type: type 'overwrite: overwrite)
             output-fmt)))
        ((string=? action "get-parameter")
         (let ((name (or (get-opt args "--name")
                         (error 'ssm "get-parameter requires --name")))
               (decrypt (get-flag args "--with-decryption")))
           (output-result
             (get-parameter client name 'with-decryption: decrypt)
             output-fmt)))
        ((string=? action "get-parameters")
         (let ((names (parse-csv (or (get-opt args "--names") "")))
               (decrypt (get-flag args "--with-decryption")))
           (output-result
             (get-parameters client names 'with-decryption: decrypt)
             output-fmt)))
        ((string=? action "delete-parameter")
         (let ((name (or (get-opt args "--name")
                         (error 'ssm "delete-parameter requires --name"))))
           (output-result (delete-parameter client name) output-fmt)))
        ((string=? action "describe-instance-information")
         (output-result (describe-instance-information client) output-fmt))
        ((string=? action "send-command")
         (let ((ids (parse-csv (or (get-opt args "--instance-ids")
                                    (error 'ssm "send-command requires --instance-ids"))))
               (command (or (get-opt args "--command")
                            (error 'ssm "send-command requires --command")))
               (doc (or (get-opt args "--document-name") "AWS-RunShellScript"))
               (comment (get-opt args "--comment")))
           (output-result
             (send-command client ids command
               'document-name: doc
               'comment: comment)
             output-fmt)))
        ((string=? action "get-command-invocation")
         (let ((cmd-id (or (get-opt args "--command-id")
                           (error 'ssm "get-command-invocation requires --command-id")))
               (inst-id (or (get-opt args "--instance-id")
                            (error 'ssm "get-command-invocation requires --instance-id"))))
           (output-result
             (get-command-invocation client cmd-id inst-id)
             output-fmt)))
        ((or (string=? action "help") (string=? action "--help"))
         (display "jerboa-aws ssm commands:
  put-parameter                --name NAME --value VALUE [--type TYPE] [--overwrite]
  get-parameter                --name NAME [--with-decryption]
  get-parameters               --names NAME1,NAME2 [--with-decryption]
  delete-parameter             --name NAME
  describe-instance-information
  send-command                 --instance-ids IDS --command CMD [--document-name DOC] [--comment TEXT]
  get-command-invocation       --command-id ID --instance-id ID

For parallel SSM execution, use the 'pssm' command instead.
")
         (exit 0))
        (else
         (display (format "Unknown ssm action: ~a\nRun 'jerboa-aws ssm help' for available commands.\n" action))
         (exit 1)))))

  ;; ---- Main entry point ----

  (define (main . args)
    (when (or (null? args)
              (and (= (length args) 1)
                   (or (string=? (car args) "--help")
                       (string=? (car args) "-h")
                       (string=? (car args) "help"))))
      (print-usage))

    (let-values (((remaining profile region output-fmt) (parse-global-opts args)))
      (when (null? remaining)
        (print-usage))

      (let ((service (car remaining))
            (rest (cdr remaining)))

        ;; If no action given, show service help
        (when (null? rest)
          (cond
            ((string=? service "ec2") (ec2-dispatch "help" '() profile region output-fmt))
            ((string=? service "s3")    (s3-dispatch "help" '() profile region output-fmt))
            ((string=? service "s3api") (s3api-dispatch "help" '() profile region output-fmt))
            ((string=? service "sts")   (sts-dispatch "help" '() profile region output-fmt))
            ((string=? service "iam") (iam-dispatch "help" '() profile region output-fmt))
            ((string=? service "ssm") (ssm-dispatch "help" '() profile region output-fmt))
            (else (print-usage))))

        (let ((action (car rest))
              (action-args (cdr rest)))
          (cond
            ((string=? service "ec2")
             (ec2-dispatch action action-args profile region output-fmt))
            ((string=? service "s3")
             (s3-dispatch action action-args profile region output-fmt))
            ((string=? service "sts")
             (sts-dispatch action action-args profile region output-fmt))
            ((string=? service "iam")
             (iam-dispatch action action-args profile region output-fmt))
            ((string=? service "ssm")
             (ssm-dispatch action action-args profile region output-fmt))
            ;; Future services -- placeholder dispatchers
            ((member service '("lambda" "dynamodb" "logs" "sns" "sqs"
                               "cfn" "cloudwatch" "rds" "elbv2"))
             (display (format "Service '~a' is not yet implemented.\n" service))
             (exit 1))
            ((string=? service "s3api")
             (s3api-dispatch action action-args profile region output-fmt))
            (else
             (display (format "Unknown service: ~a\n" service))
             (print-usage)))))))

  ) ;; end library
