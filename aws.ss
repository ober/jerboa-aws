#!chezscheme
;; Entry point for jerboa-aws
(import (chezscheme)
        (jerboa-aws cli main))

;; Get args from AWS_ARGC/AWS_ARGn env vars (set by jerboa-aws-main.c)
;; or fall back to (command-line) for interpreted mode.
(define (get-real-args)
  (let ((argc-str (getenv "AWS_ARGC")))
    (if argc-str
      (let ((argc (string->number argc-str)))
        (let loop ((i 0) (acc '()))
          (if (>= i argc)
            (reverse acc)
            (let ((val (getenv (format "AWS_ARG~a" i))))
              (loop (+ i 1) (cons (or val "") acc))))))
      (let ((cmdline (command-line)))
        (if (pair? cmdline) (cdr cmdline) '())))))

(apply main (get-real-args))
