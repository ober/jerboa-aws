#!chezscheme
;; Build a native jerboa-aws binary.
;;
;; Usage: cd jerboa-aws && make binary

(import (chezscheme))

;; --- Helper: generate C header from binary file ---
(define (file->c-header input-path output-path array-name size-name)
  (let* ((port (open-file-input-port input-path))
         (data (get-bytevector-all port))
         (size (bytevector-length data)))
    (close-port port)
    (call-with-output-file output-path
      (lambda (out)
        (fprintf out "/* Auto-generated */~n")
        (fprintf out "static const unsigned char ~a[] = {~n" array-name)
        (let loop ((i 0))
          (when (< i size)
            (when (= 0 (modulo i 16)) (fprintf out "  "))
            (fprintf out "0x~2,'0x" (bytevector-u8-ref data i))
            (when (< (+ i 1) size) (fprintf out ","))
            (when (= 15 (modulo i 16)) (fprintf out "~n"))
            (loop (+ i 1))))
        (fprintf out "~n};~n")
        (fprintf out "static const unsigned int ~a = ~a;~n" size-name size))
      'replace)
    (printf "  ~a: ~a bytes~n" output-path size)))

;; --- Locate Chez install directory ---
(define chez-dir
  (or (getenv "CHEZ_DIR")
      (let* ((mt (symbol->string (machine-type)))
             (home (getenv "HOME"))
             (lib-dir (format "~a/.local/lib" home))
             (csv-dir
               (let lp ((dirs (guard (e (#t '())) (directory-list lib-dir))))
                 (cond
                   ((null? dirs) #f)
                   ((and (> (string-length (car dirs)) 3)
                         (string=? "csv" (substring (car dirs) 0 3)))
                    (format "~a/~a/~a" lib-dir (car dirs) mt))
                   (else (lp (cdr dirs)))))))
        (and csv-dir
             (file-exists? (format "~a/main.o" csv-dir))
             csv-dir))))

(unless chez-dir
  (display "Error: Cannot find Chez install dir. Set CHEZ_DIR.\n")
  (exit 1))

;; --- Locate gherkin runtime ---
(define gherkin-dir
  (or (getenv "GHERKIN_DIR")
      (let ((home (getenv "HOME")))
        (format "~a/mine/gherkin/src" home))))

(unless (file-exists? (format "~a/compat/types.so" gherkin-dir))
  (printf "Error: Cannot find gherkin runtime at ~a~n" gherkin-dir)
  (exit 1))

(printf "Chez dir:    ~a~n" chez-dir)
(printf "Gherkin dir: ~a~n" gherkin-dir)

(printf "
[1/6] Compiling all modules...
")
(parameterize ([compile-imported-libraries #t])
  (compile-program "aws.ss"))

(printf "[2/6] Using compiled program...
")
(system "cp aws.so jerboa-aws-all.so")

(printf "[3/6] Creating libs-only boot file...
")
(apply make-boot-file "jerboa-aws.boot" '("scheme" "petite")
  (append
    (list
      ;; Gherkin runtime
      (format "~a/compat/types.so" gherkin-dir)
      (format "~a/runtime/util.so" gherkin-dir)
      (format "~a/runtime/table.so" gherkin-dir)
      (format "~a/runtime/c3.so" gherkin-dir)
      (format "~a/runtime/mop.so" gherkin-dir)
      (format "~a/runtime/error.so" gherkin-dir)
      (format "~a/runtime/hash.so" gherkin-dir)
      (format "~a/runtime/syntax.so" gherkin-dir)
      (format "~a/runtime/eval.so" gherkin-dir)
      (format "~a/reader/reader.so" gherkin-dir)
      (format "~a/compiler/compile.so" gherkin-dir)
      (format "~a/boot/gherkin.so" gherkin-dir)
    )
    ;; Core modules
    (map (lambda (m) (format "lib/jerboa-aws/~a.so" m))
      '(creds crypto sigv4 uri time xml json request api json-api))
    ;; EC2
    (map (lambda (m) (format "lib/jerboa-aws/ec2/~a.so" m))
      '(xml params api instances security-groups vpcs subnets
        volumes snapshots addresses network-interfaces key-pairs
        images regions tags route-tables internet-gateways
        nat-gateways launch-templates))
    ;; S3
    (map (lambda (m) (format "lib/jerboa-aws/s3/~a.so" m))
      '(xml api buckets objects))
    ;; STS
    (map (lambda (m) (format "lib/jerboa-aws/sts/~a.so" m))
      '(api operations))
    ;; IAM
    (map (lambda (m) (format "lib/jerboa-aws/iam/~a.so" m))
      '(api users groups roles policies access-keys))
    ;; Lambda
    (map (lambda (m) (format "lib/jerboa-aws/lambda/~a.so" m))
      '(api functions))
    ;; Logs
    (map (lambda (m) (format "lib/jerboa-aws/logs/~a.so" m))
      '(api operations))
    ;; DynamoDB
    (map (lambda (m) (format "lib/jerboa-aws/dynamodb/~a.so" m))
      '(api operations))
    ;; SNS
    (map (lambda (m) (format "lib/jerboa-aws/sns/~a.so" m))
      '(api operations))
    ;; SQS
    (map (lambda (m) (format "lib/jerboa-aws/sqs/~a.so" m))
      '(api operations))
    ;; CloudFormation
    (map (lambda (m) (format "lib/jerboa-aws/cfn/~a.so" m))
      '(api stacks))
    ;; CloudWatch
    (map (lambda (m) (format "lib/jerboa-aws/cloudwatch/~a.so" m))
      '(api operations))
    ;; RDS
    (map (lambda (m) (format "lib/jerboa-aws/rds/~a.so" m))
      '(api db-instances))
    ;; ELBv2
    (map (lambda (m) (format "lib/jerboa-aws/elbv2/~a.so" m))
      '(api operations))
    ;; SSM
    (map (lambda (m) (format "lib/jerboa-aws/ssm/~a.so" m))
      '(api operations))
    ;; Compute Optimizer
    (map (lambda (m) (format "lib/jerboa-aws/compute-optimizer/~a.so" m))
      '(api operations))
    ;; Cost Optimization Hub
    (map (lambda (m) (format "lib/jerboa-aws/cost-optimization-hub/~a.so" m))
      '(api operations))
    ;; CLI
    (map (lambda (m) (format "lib/jerboa-aws/cli/~a.so" m))
      '(format main))))

(printf "[4/6] Embedding boot files + program as C headers...
")
(file->c-header "jerboa-aws-all.so" "jerboa_aws_program.h"
                "jerboa_aws_program_data" "jerboa_aws_program_size")
(file->c-header (format "~a/petite.boot" chez-dir) "jerboa_aws_petite_boot.h"
                "petite_boot_data" "petite_boot_size")
(file->c-header (format "~a/scheme.boot" chez-dir) "jerboa_aws_scheme_boot.h"
                "scheme_boot_data" "scheme_boot_size")
(file->c-header "jerboa-aws.boot" "jerboa_aws_app_boot.h"
                "jerboa_aws_app_boot_data" "jerboa_aws_app_boot_size")

(printf "[5/6] Compiling and linking...
")
(let ((cmd (format "gcc -c -O2 -o jerboa-aws-main.o jerboa-aws-main.c -I~a -I. -Wall 2>&1" chez-dir)))
  (unless (= 0 (system cmd))
    (display "Error: C compilation failed\n")
    (exit 1)))
(let ((cmd (format "gcc -rdynamic -o jerboa-aws jerboa-aws-main.o -L~a -lkernel -llz4 -lz -lm -ldl -lpthread -luuid -lncurses -Wl,-rpath,~a"
             chez-dir chez-dir)))
  (printf "  ~a~n" cmd)
  (unless (= 0 (system cmd))
    (display "Error: Link failed\n")
    (exit 1)))

(printf "[6/6] Cleaning up...
")
(for-each (lambda (f)
            (when (file-exists? f) (delete-file f)))
  '("jerboa-aws-main.o" "jerboa_aws_program.h"
    "jerboa_aws_petite_boot.h" "jerboa_aws_scheme_boot.h" "jerboa_aws_app_boot.h"
    "jerboa-aws-all.so" "aws.so" "aws.wpo" "jerboa-aws.boot"))

(printf "
========================================
")
(printf "Build complete!

")
(printf "  Binary: ./jerboa-aws  (~a KB)
"
  (quotient (file-length (open-file-input-port "jerboa-aws")) 1024))
