#!chezscheme
;;; (jerboa-aws s3 xml) -- S3 XML parsing (re-export from jerboa-aws xml)

(library (jerboa-aws s3 xml)
  (export xml-parse sxml->hash sxml-text sxml-items
          strip-ns aws-response->hash)
  (import (chezscheme)
          (jerboa-aws xml))

  ) ;; end library
