#!chezscheme
;; Entry point for pssm (Parallel SSM)
(import (chezscheme)
        (jerboa-aws pssm))

;; Get args from PSSM_ARGC/PSSM_ARGn env vars (set by C wrapper)
;; or fall back to (command-line) for interpreted mode.
(define (get-real-args)
  (let ((argc-str (getenv "PSSM_ARGC")))
    (if argc-str
      (let ((argc (string->number argc-str)))
        (let loop ((i 0) (acc '()))
          (if (>= i argc)
            (reverse acc)
            (let ((val (getenv (format "PSSM_ARG~a" i))))
              (loop (+ i 1) (cons (or val "") acc))))))
      (let ((cmdline (command-line)))
        (if (pair? cmdline) (cdr cmdline) '())))))

(apply pssm-main (get-real-args))
