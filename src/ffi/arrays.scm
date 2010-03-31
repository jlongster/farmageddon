
(include "arrays#.scm")
(include "ffi#.scm")
(include "../lib/tests.scm")

;;; util

(define NULL
  ((c-lambda () (pointer void #f) "___result_voidstar=0;")))

(define free
  (c-lambda ((pointer void #f)) void "free((void*)___arg1);"))

(define ->void-array
  (c-lambda ((pointer void #f)) (pointer void #f)
            "___result_voidstar = (void*)___arg1;"))

;;; unsigned-int8-array

(define make-unsigned-int8-array
  (c-lambda (int) unsigned-int8-array
            "___result_voidstar = malloc(___arg1*sizeof(unsigned char));"))

(define unsigned-int8-array-ref
  (c-lambda (unsigned-int8-array int) unsigned-int8
            "___result = ((unsigned char*)___arg1)[___arg2];"))

(define unsigned-int8-array-set!
  (c-lambda (unsigned-int8-array int unsigned-int8) void
            "((unsigned char*)___arg1)[___arg2] = ___arg3;"))

(define (vector->unsigned-int8-array vec)
  (let* ((length (vector-length vec))
         (buf (make-unsigned-int8-array length)))
    (let loop ((i 0))
      (if (< i length)
          (begin
            (unsigned-int8-array-set! buf i (vector-ref vec i))
            (loop (+ i 1)))
          buf))))

;;; unsigned-int16-array

(define make-unsigned-int16-array
  (c-lambda (int) unsigned-int16-array
            "___result_voidstar = malloc(___arg1*sizeof(unsigned short));"))

(define unsigned-int16-array-ref
  (c-lambda (unsigned-int16-array int) unsigned-int16
            "___result = ((unsigned short*)___arg1)[___arg2];"))

(define unsigned-int16-array-set!
  (c-lambda (unsigned-int16-array int unsigned-int16) void
            "((unsigned short*)___arg1)[___arg2] = ___arg3;"))

(define (vector->unsigned-int16-array vec)
  (let* ((length (vector-length vec))
         (buf (make-unsigned-int16-array length)))
    (let loop ((i 0))
      (if (< i length)
          (begin
            (unsigned-int16-array-set! buf i (vector-ref vec i))
            (loop (+ i 1)))
          buf))))

;;; unsigned-int array

(define make-unsigned-int-array
  (c-lambda (int) unsigned-int-array
            "___result_voidstar = malloc(___arg1*sizeof(unsigned int));"))

(define unsigned-int-array-ref
  (c-lambda (unsigned-int-array int) unsigned-int
            "___result = ((unsigned int*)___arg1)[___arg2];"))

(define unsigned-int-array-set!
  (c-lambda (unsigned-int-array int unsigned-int) void
            "((unsigned int*)___arg1)[___arg2] = ___arg3;"))

(define (vector->unsigned-int-array vec)
  (let* ((length (vector-length vec))
         (buf (make-unsigned-int-array length)))
    (let loop ((i 0))
      (if (< i length)
          (begin
            (unsigned-int-array-set! buf i (vector-ref vec i))
            (loop (+ i 1)))
          buf))))

;; (with-alloc (buf (make-unsigned-int-array 5))
;;   (unsigned-int-array-set! buf 0 70000)
;;   (unsigned-int-array-set! buf 1 80000)
;;   (assert-equal (unsigned-int-array-ref buf 0) 70000)
;;   (assert-equal (unsigned-int-array-ref buf 1) 80000))

;; (with-alloc (buf (vector->unsigned-int-array (vector 1000000 2000000)))
;;   (assert-equal (unsigned-int-array-ref buf 0) 1000000)
;;   (assert-equal (unsigned-int-array-ref buf 1) 2000000))

;;; int array

(define make-int-array
  (c-lambda (int) int-array
            "___result_voidstar = malloc(___arg1*sizeof(int));"))

(define int-array-ref
  (c-lambda (int-array int) int
            "___result = ((int*)___arg1)[___arg2];"))



;;; float

(define make-float-array
  (c-lambda (int) float-array
            "___result_voidstar = malloc(___arg1*sizeof(float));"))

(define float-array-ref
  (c-lambda (float-array int) float
            "___result = ((float*)___arg1)[___arg2];"))

(define float-array-set!
  (c-lambda (float-array int float) void
            "((float*)___arg1)[___arg2] = ___arg3;"))

(define (vector->float-array vec)
  (declare (inlining-limit 10000))
  (let* ((length (vector-length vec))
         (buf (make-float-array length)))
    (let loop ((i 0))
      (if (< i length)
          (begin
            (float-array-set! buf i (vector-ref vec i))
            (loop (+ i 1)))
          buf))))

;; (with-alloc (buf (make-float-array 5))
;;   (float-array-set! buf 0 1.)
;;   (float-array-set! buf 1 5.)
;;   (assert-equal (float-array-ref buf 0) 1.)
;;   (assert-equal (float-array-ref buf 1) 5.))

;; (with-alloc (buf (vector->float-array (vector 1. 2. 3.)))
;;   (assert-equal (float-array-ref buf 0) 1.)
;;   (assert-equal (float-array-ref buf 1) 2.)
;;   (assert-equal (float-array-ref buf 2) 3.))

;; (define-macro (implement-c-array name scheme-type c-type)
;;   (define make-symbol
;;     (lambda args
;;       (string->symbol
;;        (apply string-append (map (lambda (el)
;;                                    (if (symbol? el)
;;                                        (symbol->string el)
;;                                        el))
;;                                  args)))))
  
;;   `(begin
;;      (define ,(make-symbol "make-" name "-array")
;;        (c-lambda (,scheme-type) ))))
