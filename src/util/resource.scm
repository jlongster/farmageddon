;;;; "resources"
;;; This file implements two ways of finding resources: one calls into
;;; Cocoa to find the resource path of the current bundle, and the
;;; other depends on "config.scm" to define a variable named "root".
;;;
;;; The former seems more scalable, but it requires up to deploy all of
;;; our resources like Xcode, which is annoying, so we choose the
;;; latter.

(define (resource path)
  (let ((base (NSBundle-resource-path (NSBundle-main-bundle))))
    (string-append base "/" path)))

(define (local-resource path)
  (string-append root "/" path))
