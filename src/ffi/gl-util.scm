
(define PI 3.14159265)

(define current-perspective (make-parameter #f))

(define-type perspective
  xmin
  xmax
  ymin
  ymax
  znear
  zfar)

(define (frustum fovy aspect znear zfar)
  (let* ((xmax (* znear (tan (/ (* fovy PI) 360.))))
         (xmin (- xmax))
         (ymin (/ xmin aspect))
         (ymax (/ xmax aspect)))
    (list xmin xmax ymin ymax znear zfar)))

(define (perspective fovy aspect znear zfar)
  (let ((vals (frustum fovy aspect znear zfar)))
    (current-perspective
     (apply make-perspective vals))
    (apply glFrustumf vals)))

(define (ortho xmin xmax ymin ymax znear zfar)
  (current-perspective
   (make-perspective xmin xmax ymin ymax znear zfar))
  (glOrthof xmin xmax ymin ymax znear zfar))

(define (lookat eye center up)
  (glMultMatrixf (lookat-matrix eye center up))
  (glTranslatef (- (vec3d-x eye))
                (- (vec3d-y eye))
                (- (vec3d-z eye))))

(define (lookat-matrix eye center up)
  (let* ((z (vec3d-unit (vec3d-sub center eye)))
         (x (vec3d-unit (vec3d-cross z up)))
         (y (vec3d-unit (vec3d-cross x z))))
    (vector->float-array
     (vector (vec3d-x x) (vec3d-y x) (vec3d-z x) 0.
             (vec3d-x y) (vec3d-y y) (vec3d-z y) 0.
             (- (vec3d-x z)) (- (vec3d-y z)) (- (vec3d-z z)) 0.
             0.          0.          0.          1.))))
