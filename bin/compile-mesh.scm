#!/usr/bin/env gsi-script

(include "../src/lib/srfi/srfi-1.scm")
(include "../src/lib/vectors.scm")
(include "../src/lib/obj-loader.scm")

(define (main filename)
  (compress (string-append filename ".obj.gso") (obj-load filename #f #t)))
