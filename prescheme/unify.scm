
(define-syntax unify-vector
  (syntax-rules ()
    ((_ var)
     (vector-ref var 0))))

(define-syntax unify-string
  (syntax-rules ()
    ((_ var)
     (string-ref var 0))))

(define-syntax unify-int
  (syntax-rules ()
    ((_ var)
     (+ 0 var))))
