#!chezscheme
;;; (jerboa-aws request) -- HTTPS HTTP client via chez-https (native TLS)
;;; Re-exports chez-https API for AWS API calls

(library (jerboa-aws request)
  (export http-get http-post http-put http-delete http-head
          request-status request-text request-content
          request-headers request-close)
  (import (chezscheme) (chez-https))

  ) ;; end library
