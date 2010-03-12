
(declare (block)
         (standard-bindings)
         (extended-bindings))

(define (make-quaternion x y z w) (f64vector x y z w))
(define (quaternion-x quat) (f64vector-ref quat 0))
(define (quaternion-y quat) (f64vector-ref quat 1))
(define (quaternion-z quat) (f64vector-ref quat 2))
(define (quaternion-w quat) (f64vector-ref quat 3))

(define (quaternion-unit quat)
  (let* ((x (quaternion-x quat))
         (y (quaternion-y quat))
         (z (quaternion-z quat))
         (w (quaternion-w quat))
         (len (+ (* x x)
                 (* y y)
                 (* z z)
                 (* w w))))
    (make-quaternion (/ x len)
                     (/ y len)
                     (/ z len)
                     (/ w len))))

(define (quaternion-conjugate quat)
  (make-quaternion (- (quaternion-x quat))
                   (- (quaternion-y quat))
                   (- (quaternion-z quat))
                   (quaternion-w quat)))

(define (quaternion-mul quat1 quat2)
  (let* ((x1 (quaternion-x quat1))
         (y1 (quaternion-y quat1))
         (z1 (quaternion-z quat1))
         (w1 (quaternion-w quat1))
         
         (x2 (quaternion-x quat2))
         (y2 (quaternion-y quat2))
         (z2 (quaternion-z quat2))
         (w2 (quaternion-w quat2)))
    (make-quaternion
     (- (+ (* w1 x2) (* x1 w2) (* y1 z2)) (* z1 y2))
     (- (+ (* w1 y2) (* y1 w2) (* z1 x2)) (* x1 z2))
     (- (+ (* w1 z2) (* z1 w2) (* x1 y2)) (* y1 x2))
     (- (* w1 w2) (* x1 x2) (* y1 y2) (* z1 z2)))))

(define (quaternion-rotate quat vec)
  (let* ((vec-quat (make-quaternion (vec3d-x vec)
                                    (vec3d-y vec)
                                    (vec3d-z vec)
                                    0.))
         (res (quaternion-mul
               quat
               (quaternion-mul
                vec-quat
                (quaternion-conjugate quat)))))
    (make-vec3d (quaternion-x res)
                (quaternion-y res)
                (quaternion-z res))))

(define (quaternion-axisangle deg x y z)
  (let* ((angle (/ (/ (* deg PI) 180.) 2.))
         (angle-sin (sin angle)))
    (make-quaternion (* x angle-sin)
                     (* y angle-sin)
                     (* z angle-sin)
                     (cos angle))))

