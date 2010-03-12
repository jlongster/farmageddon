
(declare (block)
         (standard-bindings)
         (extended-bindings))

(define PI 3.14159265)

(define-type perspective
  matrix
  xmin
  xmax
  ymin
  ymax
  znear
  zfar)

(define (frustum-clipping-planes fovy aspect znear zfar)
  (let* ((xmax (* znear (tan (/ (* fovy PI) 360.))))
         (xmin (- xmax))
         (ymin (/ xmin aspect))
         (ymax (/ xmax aspect)))
    (list xmin xmax ymin ymax znear zfar)))

(define (frustum-matrix xmin xmax ymin ymax znear zfar)
  (4x4matrix (/ (* 2 znear) (- xmax xmin)) 0.
             (/ (+ xmax xmin) (- xmax xmin)) 0.

             0. (/ (* 2 znear) (- ymax ymin))
             (/ (+ ymax ymin) (- ymax ymin)) 0.

             0. 0.
             (- (/ (+ zfar znear)
                   (- zfar znear)))
             (- (/ (* 2 zfar znear)
                   (- zfar znear)))

             0. 0. -1. 0.))

(define (ortho-matrix xmin xmax ymin ymax znear zfar)
  (4x4matrix (/ 2 (- xmax xmin)) 0.
             0. (- (/ (+ xmax xmin)
                      (- xmax xmin)))
             
             0. (/ 2 (- ymax ymin))
             0. (- (/ (+ ymax ymin)
                      (- ymax ymin)))

             0. 0.
             (/ -2. (- zfar znear))
             (- (/ (+ zfar znear)
                   (- zfar znear)))
             
             0. 0. 0. 1.))

(define (lookat-matrix eye center up)
  (let* ((z (vec3d-unit (vec3d-sub center eye)))
         (x (vec3d-unit (vec3d-cross z up)))
         (y (vec3d-unit (vec3d-cross x z))))
    (4x4matrix (vec3d-x x) (vec3d-y x) (vec3d-z x) 0.
               (vec3d-x y) (vec3d-y y) (vec3d-z y) 0.
               (- (vec3d-x z)) (- (vec3d-y z)) (- (vec3d-z z)) 0.
               0.          0.          0.          1.)))

(define (perspective fovy aspect znear zfar)
  (let ((vals (frustum-clipping-planes fovy aspect znear zfar)))
    (apply make-perspective
           (cons (apply frustum-matrix vals) vals))))

(define (ortho xmin xmax ymin ymax znear zfar)
  (make-perspective (ortho-matrix xmin xmax ymin ymax znear zfar)
                    xmin xmax ymin ymax znear zfar))

(define (lookat eye center up)
  ;; todo: apply eye by translating this matrix (we are always using
  ;; (0, 0, 0) right now)
  (lookat-matrix eye center up))

;; crappy 4x4 matrix implementation

(define (4x4matrix . vals)
  (let ((vals (list->vector vals)))
    (define (ref i)
      (vector-ref vals i))
    
    (vector->float-array
     (vector (ref 0) (ref 4) (ref 8) (ref 12)
             (ref 1) (ref 5) (ref 9) (ref 13)
             (ref 2) (ref 6) (ref 10) (ref 14)
             (ref 3) (ref 7) (ref 11) (ref 15)))))

(define (4x4matrix-multiply m1 m2)
  (define-macro (ref m i j)
    `(float-array-ref ,m ,(+ (* j 4) i)))

  (define-macro (val i j)
    `(+ (* (ref m1 0 ,i) (ref m2 ,j 0))
        (* (ref m1 1 ,i) (ref m2 ,j 1))
        (* (ref m1 2 ,i) (ref m2 ,j 2))
        (* (ref m1 3 ,i) (ref m2 ,j 3))))
  
  (vector->float-array
   (vector (val 0 0) (val 0 1) (val 0 2) (val 0 3)
           (val 1 0) (val 1 1) (val 1 2) (val 1 3)
           (val 2 0) (val 2 1) (val 2 2) (val 2 3)
           (val 3 0) (val 3 1) (val 3 2) (val 3 3))))

(define (4x4matrix-display m)
  (let loop ((i 0))
    (if (< i 4)
        (begin
          (let loop ((j 0))
            (if (< j 4)
                (begin
                  (display
                   (float-array-ref m (+ (* j 4) i)))
                  (display " ")
                  (loop (+ j 1)))))
          (newline)
          (loop (+ i 1))))))
