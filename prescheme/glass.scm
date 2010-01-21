
(define PI 3.1415926536)

(define (rotate x y degrees)
  (let* ((rad (* (/ degrees 180.) PI))
         (rad-sin (sin rad))
         (rad-cos (cos rad)))
    (cons
     (- (* x rad-cos)
        (* y rad-sin))
     (+ (* x rad-sin)
        (* y rad-cos)))))

(define (crack x y)
  (rotate x y 30.))

(define (main argc argv)
  (unify-string (unify-vector argv))
  (unify-int argc)
  (crack 1. 2.))

