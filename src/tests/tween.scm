
;; scale

(begin
  (define tx (image-opengl-load "survived.png"))

  (scene-list-clear!)
  (scene-list-add
   (make-tween
    (make-2d-object
     2d-perspective
     color: (make-vec4d 1. 1. 1. 1.)
     scale: (make-vec3d 1. .3 1.)
     texture: tx)
    length: 1.
    color: (make-vec4d 0. 3. 5. 1.)
    scale: (make-vec3d 1.5 1.5 1.))))

;; grid cells

(begin
  (scene-list-clear!)

  (define (random-vec4d)
    (make-vec4d (random-real)
                (random-real)
                (random-real)
                (random-real)))
  
  (define GRID-X 10.)
  (define GRID-Y 15.)

  (define (tweened-cell x y)
    (let ((width (UIView-width (current-view)))
          (height (UIView-height (current-view))))
      (make-tween
       (make-2d-object
        2d-perspective
        color: (random-vec4d)
        position: (make-vec3d (/ x GRID-X)
                              (* (/ y GRID-Y) 1.5)
                              0.)
        scale: (make-vec3d (/ GRID-X)
                           (/ 1.5 GRID-Y)
                           1.))
       length: (* (random-real) 3.)
       color: (make-vec4d (random-real)
                          (random-real)
                          (random-real)
                          0.))))

  (let loop ((i 0))
    (if (< i (* GRID-X GRID-Y))
        (begin
          (scene-list-add
           (tweened-cell (exact->inexact (remainder i GRID-X))
                         (exact->inexact (floor (/ i GRID-Y)))))
          (loop (+ i 1))))))

;; position

(begin
  (scene-list-clear!)

  (define cursor 0)
  (define types '(linear
                  ease-in-quad ease-in-cubic
                  ease-out-quad ease-out-cubic
                  ease-inout-quad ease-inout-cubic))
  
  (for-each
   (lambda (type)
     (let ((x (+ .1
                 (* (/ cursor (length types)) .9))))
       (scene-list-add
        (make-tween
         (make-2d-object
          2d-perspective
          position: (make-vec3d x .2 0.)
          scale: (make-vec3d .05 (/ .05 1.5) 1.)
          color: (make-vec4d 1. 1. 1. 1.))
         position: (make-vec3d x .8 0.)
         color: (make-vec4d (+ (* (random-real) .5) .5)
                            (+ (* (random-real) .5) .5)
                            (+ (* (random-real) .5) .5)
                            (+ (* (random-real) .5) .5))
         length: 1.
         type: type)))
     (set! cursor (+ cursor 1)))
   types))

(begin
  (scene-list-clear!)

  (define tx (image-opengl-load "survived.png"))
  
  (scene-list-add
   (make-tween
    
    (make-2d-object
     2d-ratio-perspective
     position: (make-vec3d (- .5 .025) .1 0.)
     scale: (make-vec3d .05 .05 1.)
     color: (make-vec4d 1. 1. 1. 1.)
     rotation: (make-vec4d 0. 0. 1. 0.)
     ;;texture: tx
     )
    
    position: (make-vec3d (- .5 .025) .5 0.)
    scale: (make-vec3d .4 .4 1.)
    color: (make-vec4d 0. 1. 0. .5)
    rotation: (make-vec4d 0. 0. 1. 90.)
    
    length: 1.5)))

