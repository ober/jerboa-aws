#!chezscheme
;;; (jerboa-aws pssm) -- Parallel SSM: run commands on EC2 instances via AWS SSM
;;; Reads ~/.aws-ec2-cache.json, matches instances by glob pattern,
;;; sends SSM commands in batches, polls for results.

(library (jerboa-aws pssm)
  (export pssm-main)
  (import (chezscheme)
          (jerboa-aws json)
          (jerboa-aws ssm api)
          (jerboa-aws ssm operations))

  ;; ---- Error formatting ----
  (define (condition->string e)
    (if (condition? e)
      (call-with-string-output-port
        (lambda (p) (display-condition e p)))
      (format "~a" e)))

  ;; ---- Constants ----
  (define SSM-BATCH-SIZE 50)
  (define DEFAULT-TIMEOUT 60)     ;; seconds
  (define DEFAULT-POLL-INTERVAL 2) ;; seconds

  ;; ---- Hashtable helpers ----
  (define (ht-ref ht key . default)
    (if (hashtable? ht)
      (guard (e [#t (if (pair? default) (car default) #f)])
        (hashtable-ref ht key (if (pair? default) (car default) #f)))
      (if (pair? default) (car default) #f)))

  (define (ht-ref/str ht key)
    (let ([v (ht-ref ht key "")])
      (if (string? v) v "")))

  ;; ---- Instance cache ----

  (define (default-cache-file)
    (string-append (getenv "HOME") "/.aws-ec2-cache.json"))

  (define (load-instance-cache path)
    (guard (e [#t (error 'pssm
                    (string-append "Cannot read cache: " path
                      "\nHint: Run 'pssh --refresh' or 'pssm --refresh' to populate the cache"))])
      (let ([data (call-with-input-file path read-json)])
        ;; data is a list of hashtables; filter to running only
        (filter (lambda (inst)
                  (string=? (ht-ref/str inst "state") "running"))
                (if (list? data) data
                    (if (vector? data) (vector->list data) '()))))))

  ;; ---- Glob matching ----
  ;; Converts shell glob pattern to a matcher: * → match any sequence
  ;; Case-insensitive

  (define (str-downcase str)
    (let ([len (string-length str)])
      (let ([out (make-string len)])
        (do ([i 0 (+ i 1)]) ((= i len) out)
          (string-set! out i (char-downcase (string-ref str i)))))))

  (define (glob-match? pattern str)
    (glob-chars-match? (string->list (str-downcase pattern))
                       (string->list (str-downcase str))))

  (define (glob-chars-match? pat str)
    (cond
      [(and (null? pat) (null? str)) #t]
      [(null? pat) #f]
      [(char=? (car pat) #\*)
       (if (null? (cdr pat))
         #t  ;; trailing * matches everything
         (or (glob-chars-match? (cdr pat) str)          ;; * matches nothing
             (and (pair? str)
                  (glob-chars-match? pat (cdr str)))))]  ;; * matches one more char
      [(null? str) #f]
      [(char=? (car pat) (car str))
       (glob-chars-match? (cdr pat) (cdr str))]
      [else #f]))

  (define (filter-instances instances pattern)
    (let ([matched (filter (lambda (inst)
                             (glob-match? pattern (ht-ref/str inst "name")))
                           instances)])
      (list-sort (lambda (a b)
                   (string<? (ht-ref/str a "name") (ht-ref/str b "name")))
                 matched)))

  ;; ---- Group by region ----

  (define (group-by-region instances)
    ;; Returns alist of (region . instances-list)
    (let ([ht (make-hashtable string-hash string=?)])
      (for-each
        (lambda (inst)
          (let ([region (ht-ref/str inst "region")])
            (hashtable-set! ht region
              (cons inst (hashtable-ref ht region '())))))
        instances)
      (let-values ([(keys vals) (hashtable-entries ht)])
        (let loop ([i 0] [acc '()])
          (if (= i (vector-length keys))
            acc
            (loop (+ i 1)
                  (cons (cons (vector-ref keys i)
                              (reverse (vector-ref vals i)))
                        acc)))))))

  ;; ---- Batching ----

  (define (batch-list lst size)
    (let loop ([rest lst] [acc '()])
      (if (null? rest)
        (reverse acc)
        (let take ([r rest] [n 0] [batch '()])
          (if (or (null? r) (= n size))
            (loop r (cons (reverse batch) acc))
            (take (cdr r) (+ n 1) (cons (car r) batch)))))))

  ;; ---- Time helpers ----

  (define (current-seconds)
    (let ([t (current-time 'time-utc)])
      (+ (time-second t)
         (/ (time-nanosecond t) 1000000000.0))))

  (define (sleep-seconds n)
    (sleep (make-time 'time-duration 0 n)))

  ;; ---- SSM Execution ----

  ;; Result record: (name instance-id region host command-id status
  ;;                 stdout stderr exit-code error duration)

  (define (make-result inst)
    (let ([r (make-hashtable string-hash string=?)])
      (hashtable-set! r "name" (ht-ref/str inst "name"))
      (hashtable-set! r "instance_id" (ht-ref/str inst "instance_id"))
      (hashtable-set! r "region" (ht-ref/str inst "region"))
      (hashtable-set! r "host" (ht-ref/str inst "private_ip"))
      (hashtable-set! r "command_id" "")
      (hashtable-set! r "status" "Pending")
      (hashtable-set! r "stdout" "")
      (hashtable-set! r "stderr" "")
      (hashtable-set! r "exit_code" -1)
      (hashtable-set! r "error" "")
      (hashtable-set! r "duration" 0.0)
      (hashtable-set! r "start_time" (current-seconds))
      r))

  (define (result-complete? r)
    (member (ht-ref/str r "status")
            '("Success" "Failed" "Cancelled" "TimedOut")))

  (define (execute-ssm instances config)
    (let* ([pattern (ht-ref/str config "pattern")]
           [command (ht-ref/str config "command")]
           [profile (ht-ref config "profile")]
           [timeout (or (ht-ref config "timeout") DEFAULT-TIMEOUT)]
           [poll-interval (or (ht-ref config "poll-interval") DEFAULT-POLL-INTERVAL)]
           [document-name (or (ht-ref config "document-name") "AWS-RunShellScript")]
           [comment (ht-ref config "comment")]
           [verbose (ht-ref config "verbose")]
           [start-time (current-seconds)]
           [by-region (group-by-region instances)]
           [all-results '()])

      ;; For each region: create client, send commands, collect results
      (for-each
        (lambda (region-pair)
          (let* ([region (car region-pair)]
                 [region-instances (cdr region-pair)]
                 [client (apply SSMClient
                           (append
                             (list 'region: region)
                             (if profile (list 'profile: profile) '())))]
                 [batches (batch-list region-instances SSM-BATCH-SIZE)]
                 [region-results (map make-result region-instances)]
                 ;; Build instance-id → result mapping
                 [result-map (let ([ht (make-hashtable string-hash string=?)])
                               (for-each
                                 (lambda (r)
                                   (hashtable-set! ht (ht-ref/str r "instance_id") r))
                                 region-results)
                               ht)]
                 ;; Track batch info: list of (command-id . instance-ids)
                 [batch-infos '()])

            (when verbose
              (display (format "  [~a] Sending ~a batch~a (~a instances)...\n"
                        region (length batches)
                        (if (= (length batches) 1) "" "es")
                        (length region-instances)))
              (flush-output-port (current-output-port)))

            ;; Send all batches
            (for-each
              (lambda (batch)
                (let ([ids (map (lambda (inst) (ht-ref/str inst "instance_id")) batch)])
                  (guard (e [#t
                    ;; Mark all in batch as failed
                    (for-each
                      (lambda (id)
                        (let ([r (hashtable-ref result-map id #f)])
                          (when r
                            (hashtable-set! r "status" "Failed")
                            (hashtable-set! r "error"
                              (format "Failed to send command: ~a"
                                (condition->string e)))
                            (hashtable-set! r "duration"
                              (- (current-seconds) (ht-ref r "start_time" 0))))))
                      ids)
                    (when verbose
                      (display (format "  [~a] Batch failed: ~a\n" region
                                (condition->string e))))])
                    (let* ([resp (apply send-command client ids command
                                  (append
                                    (list 'document-name: document-name)
                                    (if comment (list 'comment: comment) '())))]
                           [cmd-ht (ht-ref resp "Command")]
                           [cmd-id (if cmd-ht (ht-ref/str cmd-ht "CommandId") "")])
                      (when (string=? cmd-id "")
                        (error 'pssm "No CommandId returned from SendCommand"))
                      (when verbose
                        (display (format "  [~a] Command ID: ~a (~a instances)\n"
                                  region cmd-id (length ids))))
                      ;; Record command-id on results
                      (for-each
                        (lambda (id)
                          (let ([r (hashtable-ref result-map id #f)])
                            (when r (hashtable-set! r "command_id" cmd-id))))
                        ids)
                      (set! batch-infos
                        (cons (cons cmd-id ids) batch-infos))))))
              batches)

            ;; Poll for results
            (let poll-loop ([elapsed 0])
              (let ([pending (filter (lambda (r) (not (result-complete? r)))
                                    region-results)])
                (when (and (pair? pending) (< elapsed timeout))
                  (sleep-seconds (min poll-interval (- timeout elapsed)))
                  ;; Poll each pending instance
                  (for-each
                    (lambda (r)
                      (let ([cmd-id (ht-ref/str r "command_id")]
                            [inst-id (ht-ref/str r "instance_id")])
                        (when (and (not (string=? cmd-id ""))
                                   (not (result-complete? r)))
                          (guard (e [#t #f])  ;; Not ready yet, ignore
                            (let ([resp (get-command-invocation client cmd-id inst-id)])
                              (let ([status (ht-ref/str resp "Status")])
                                (when (member status '("Success" "Failed" "Cancelled" "TimedOut"))
                                  (hashtable-set! r "status" status)
                                  (hashtable-set! r "stdout"
                                    (or (ht-ref resp "StandardOutputContent") ""))
                                  (hashtable-set! r "stderr"
                                    (or (ht-ref resp "StandardErrorContent") ""))
                                  (hashtable-set! r "exit_code"
                                    (or (ht-ref resp "ResponseCode") -1))
                                  (hashtable-set! r "duration"
                                    (- (current-seconds) (ht-ref r "start_time" 0)))
                                  (when verbose
                                    (display (format "  [~a] ~a: ~a\n"
                                              region (ht-ref/str r "name") status))))))))))
                    pending)
                  (poll-loop (- (current-seconds) start-time)))))

            ;; Mark any still-pending as timed out
            (for-each
              (lambda (r)
                (unless (result-complete? r)
                  (hashtable-set! r "status" "TimedOut")
                  (hashtable-set! r "error" "Command timed out")
                  (hashtable-set! r "duration"
                    (- (current-seconds) (ht-ref r "start_time" 0)))))
              region-results)

            (set! all-results (append all-results region-results))))
        by-region)

      ;; Build execution result
      (let* ([end-time (current-seconds)]
             [successes (filter (lambda (r)
                                 (and (string=? (ht-ref/str r "status") "Success")
                                      (= (ht-ref r "exit_code" -1) 0)
                                      (string=? (ht-ref/str r "error") "")))
                                all-results)]
             [failures (filter (lambda (r)
                                (not (and (string=? (ht-ref/str r "status") "Success")
                                          (= (ht-ref r "exit_code" -1) 0)
                                          (string=? (ht-ref/str r "error") ""))))
                               all-results)])
        (let ([exec-result (make-hashtable string-hash string=?)])
          (hashtable-set! exec-result "pattern" pattern)
          (hashtable-set! exec-result "command" command)
          (hashtable-set! exec-result "total_hosts" (length all-results))
          (hashtable-set! exec-result "success_count" (length successes))
          (hashtable-set! exec-result "failure_count" (length failures))
          (hashtable-set! exec-result "total_duration" (- end-time start-time))
          (hashtable-set! exec-result "results" all-results)
          exec-result))))

  ;; ---- Output formatting ----

  (define (string-trim-right str ch)
    (let loop ([i (- (string-length str) 1)])
      (cond
        [(< i 0) ""]
        [(char=? (string-ref str i) ch) (loop (- i 1))]
        [else (substring str 0 (+ i 1))])))

  (define (string-split-lines str)
    (let ([len (string-length str)])
      (let loop ([i 0] [start 0] [acc '()])
        (cond
          [(= i len)
           (reverse (cons (substring str start i) acc))]
          [(char=? (string-ref str i) #\newline)
           (loop (+ i 1) (+ i 1) (cons (substring str start i) acc))]
          [else (loop (+ i 1) start acc)]))))

  (define (result-success? r)
    (and (string=? (ht-ref/str r "status") "Success")
         (= (ht-ref r "exit_code" -1) 0)
         (string=? (ht-ref/str r "error") "")))

  (define (output-pretty exec-result)
    (let* ([results (ht-ref exec-result "results")]
           [successes (filter result-success? results)]
           [failures (filter (lambda (r) (not (result-success? r))) results)])

      (display "\n")
      (display (make-string 80 #\=))
      (newline)
      (display (format "PSSM Results: ~a/~a succeeded | Pattern: ~a | Duration: ~,2fs\n"
                (ht-ref exec-result "success_count")
                (ht-ref exec-result "total_hosts")
                (ht-ref/str exec-result "pattern")
                (ht-ref exec-result "total_duration")))
      (display (make-string 80 #\=))
      (display "\n\n")

      ;; Print successes first
      (for-each (lambda (r) (print-host-result r #t)) successes)
      ;; Then failures
      (for-each (lambda (r) (print-host-result r #f)) failures)))

  (define (print-host-result r success?)
    (let ([name (ht-ref/str r "name")]
          [inst-id (ht-ref/str r "instance_id")]
          [region (ht-ref/str r "region")]
          [status (ht-ref/str r "status")]
          [stdout (ht-ref/str r "stdout")]
          [stderr (ht-ref/str r "stderr")]
          [err (ht-ref/str r "error")]
          [duration (ht-ref r "duration" 0)])
      (if success?
        (display (format "\033[32m~a [~a]\033[0m (~,2fs) [~a]\n" "OK" name duration region))
        (display (format "\033[31m~a [~a]\033[0m (~,2fs) [~a]\n" "FAIL" name duration region)))
      (display (format "  Instance: ~a | Status: ~a\n" inst-id status))
      (unless (string=? err "")
        (display (format "  \033[31mError: ~a\033[0m\n" err)))
      (unless (string=? stdout "")
        (display "  --- stdout ---\n")
        (for-each
          (lambda (line) (display (format "  | ~a\n" line)))
          (string-split-lines (string-trim-right stdout #\newline))))
      (unless (string=? stderr "")
        (display "  --- stderr ---\n")
        (for-each
          (lambda (line) (display (format "  | \033[33m~a\033[0m\n" line)))
          (string-split-lines (string-trim-right stderr #\newline))))
      (newline)))

  (define (output-json exec-result)
    (display (json-object->string exec-result))
    (newline))

  ;; ---- List instances ----

  (define (list-instances instances json?)
    (if json?
      (begin (display (json-object->string (list->vector instances))) (newline))
      (begin
        (display (format "Found ~a matching instances:\n\n" (length instances)))
        (display (format "~40a ~20a ~16a ~16a ~a\n"
                  "NAME" "INSTANCE ID" "PRIVATE IP" "PUBLIC IP" "REGION"))
        (display (make-string 120 #\-))
        (newline)
        (for-each
          (lambda (inst)
            (let ([pub (ht-ref/str inst "public_ip")])
              (display (format "~40a ~20a ~16a ~16a ~a\n"
                        (ht-ref/str inst "name")
                        (ht-ref/str inst "instance_id")
                        (ht-ref/str inst "private_ip")
                        (if (string=? pub "") "-" pub)
                        (ht-ref/str inst "region")))))
          instances))))

  ;; ---- Dry run ----

  (define (print-dry-run instances config)
    (display (format "Dry run - would execute SSM command on ~a hosts:\n\n" (length instances)))
    (display (format "Document: ~a\n" (or (ht-ref config "document-name") "AWS-RunShellScript")))
    (display (format "Command:  ~a\n" (ht-ref/str config "command")))
    (display (format "Timeout:  ~as\n\n" (or (ht-ref config "timeout") DEFAULT-TIMEOUT)))
    (let ([by-region (group-by-region instances)])
      (for-each
        (lambda (rp)
          (let* ([region (car rp)]
                 [insts (cdr rp)]
                 [num-batches (+ (quotient (length insts) SSM-BATCH-SIZE)
                                 (if (> (remainder (length insts) SSM-BATCH-SIZE) 0) 1 0))])
            (display (format "Region ~a (~a instances, ~a batch~a):\n"
                      region (length insts) num-batches
                      (if (= num-batches 1) "" "es")))
            (for-each
              (lambda (inst)
                (display (format "  - ~a (~a)\n"
                          (ht-ref/str inst "name")
                          (ht-ref/str inst "instance_id"))))
              insts)))
        by-region)))

  ;; ---- CLI argument parsing ----

  (define (parse-pssm-args args)
    (let ([config (make-hashtable string-hash string=?)])
      (hashtable-set! config "cache-file" (default-cache-file))
      (hashtable-set! config "timeout" DEFAULT-TIMEOUT)
      (hashtable-set! config "poll-interval" DEFAULT-POLL-INTERVAL)
      (hashtable-set! config "document-name" "AWS-RunShellScript")
      (let loop ([rest args] [positional '()])
        (cond
          [(null? rest)
           (let ([pos (reverse positional)])
             (when (pair? pos)
               (hashtable-set! config "pattern" (car pos)))
             (when (and (pair? pos) (pair? (cdr pos)))
               (hashtable-set! config "command"
                 (apply string-append
                   (let join ([parts (cdr pos)] [acc '()])
                     (if (null? parts) (reverse acc)
                         (join (cdr parts)
                               (if (null? acc)
                                 (list (car parts))
                                 (cons (car parts) (cons " " acc))))))))))
           config]
          ;; Flags
          [(string=? (car rest) "--json")
           (hashtable-set! config "json" #t)
           (loop (cdr rest) positional)]
          [(string=? (car rest) "-v")
           (hashtable-set! config "verbose" #t)
           (loop (cdr rest) positional)]
          [(string=? (car rest) "--verbose")
           (hashtable-set! config "verbose" #t)
           (loop (cdr rest) positional)]
          [(string=? (car rest) "-l")
           (hashtable-set! config "list-only" #t)
           (loop (cdr rest) positional)]
          [(string=? (car rest) "--dry-run")
           (hashtable-set! config "dry-run" #t)
           (loop (cdr rest) positional)]
          [(string=? (car rest) "--no-color")
           (hashtable-set! config "no-color" #t)
           (loop (cdr rest) positional)]
          ;; Options with values
          [(and (string=? (car rest) "-t") (pair? (cdr rest)))
           (hashtable-set! config "timeout" (string->number (cadr rest)))
           (loop (cddr rest) positional)]
          [(and (string=? (car rest) "--timeout") (pair? (cdr rest)))
           (hashtable-set! config "timeout" (string->number (cadr rest)))
           (loop (cddr rest) positional)]
          [(and (string=? (car rest) "-p") (pair? (cdr rest)))
           (hashtable-set! config "profile" (cadr rest))
           (loop (cddr rest) positional)]
          [(and (string=? (car rest) "--profile") (pair? (cdr rest)))
           (hashtable-set! config "profile" (cadr rest))
           (loop (cddr rest) positional)]
          [(and (string=? (car rest) "--cache") (pair? (cdr rest)))
           (hashtable-set! config "cache-file" (cadr rest))
           (loop (cddr rest) positional)]
          [(and (string=? (car rest) "--doc") (pair? (cdr rest)))
           (hashtable-set! config "document-name" (cadr rest))
           (loop (cddr rest) positional)]
          [(and (string=? (car rest) "--comment") (pair? (cdr rest)))
           (hashtable-set! config "comment" (cadr rest))
           (loop (cddr rest) positional)]
          [(and (string=? (car rest) "--poll") (pair? (cdr rest)))
           (hashtable-set! config "poll-interval" (string->number (cadr rest)))
           (loop (cddr rest) positional)]
          ;; Help
          [(or (string=? (car rest) "--help") (string=? (car rest) "-h"))
           (print-pssm-usage)
           (exit 0)]
          ;; Positional args
          [else
           (loop (cdr rest) (cons (car rest) positional))]))))

  (define (print-pssm-usage)
    (display "pssm - Parallel SSM for AWS EC2 instances

Usage: pssm [options] <pattern> [command]

Runs commands via AWS Systems Manager (no SSH required).
Requires SSM agent running on target instances.

Pattern: Glob pattern to match EC2 Name tags
         Use * as wildcard (e.g., 'web-*' matches 'web-server-1')

Examples:
  pssm 'web-*' uptime                    Run uptime on web servers
  pssm -l 'db-*'                         List matching DB servers
  pssm --json 'app-*' hostname           JSON output
  pssm -t 300 'prod-*' 'yum update -y'   Long-running command (5m timeout)
  pssm --dry-run 'critical-*' reboot     Show what would execute

Options:
  -p, --profile NAME    AWS profile name
  -t, --timeout SECS    Command timeout in seconds (default: 60)
  --poll SECS           Poll interval in seconds (default: 2)
  --doc NAME            SSM document name (default: AWS-RunShellScript)
  --comment TEXT        Comment visible in AWS console
  --cache PATH          Path to cache file (default: ~/.aws-ec2-cache.json)
  -l                    List matching instances only
  --dry-run             Show what would execute without running
  --json                Output results as JSON
  -v, --verbose         Verbose output
  --no-color            Disable ANSI colors
  -h, --help            Show this help
"))

  ;; ---- Main entry point ----

  (define (pssm-main . args)
    (when (null? args)
      (print-pssm-usage)
      (exit 1))

    (let* ([config (parse-pssm-args args)]
           [cache-file (ht-ref/str config "cache-file")]
           [pattern (ht-ref config "pattern")]
           [command (ht-ref config "command")]
           [list-only (ht-ref config "list-only")]
           [dry-run (ht-ref config "dry-run")]
           [json? (ht-ref config "json")]
           [verbose (ht-ref config "verbose")])

      (unless pattern
        (display "Error: No pattern specified\n" (current-error-port))
        (print-pssm-usage)
        (exit 1))

      ;; Load and filter instances
      (let* ([instances (load-instance-cache cache-file)]
             [matched (filter-instances instances pattern)])

        (when (null? matched)
          (display (format "No instances match pattern: ~a\n" pattern)
                   (current-error-port))
          (exit 1))

        (when verbose
          (display (format "Matched ~a instances for pattern '~a'\n"
                    (length matched) pattern)))

        ;; List only
        (when list-only
          (list-instances matched json?)
          (exit 0))

        ;; Need a command for execution
        (unless command
          (display "Error: No command specified\n" (current-error-port))
          (print-pssm-usage)
          (exit 1))

        ;; Dry run
        (when dry-run
          (print-dry-run matched config)
          (exit 0))

        ;; Execute
        (let ([result (execute-ssm matched config)])
          (if json?
            (output-json result)
            (output-pretty result))
          (when (> (ht-ref result "failure_count" 0) 0)
            (exit 1))))))

  ) ;; end library
