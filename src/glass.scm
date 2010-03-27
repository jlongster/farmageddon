;;;; glass simulation
;;; Simulates a piece of glass which cracks 

(declare (block)
         (standard-bindings)
         (extended-bindings))

(define-type crack
  id: 6B57C01C-D776-4455-BA06-8F52567184DF
  constructor: really-make-crack
  vertex-buffer
  color-buffer
  num-vertices)

(define PRELOADED-CRACKS '())
(define EXISTING-CRACKS '())
(define NUM-PRELOADED 10)
(define NUM-LINES 20)
(define LINE-SPREAD 2)

(define (load-randomized-cracks)
  (set! PRELOADED-CRACKS
        (unfold (lambda (i) (>= i NUM-PRELOADED))
                (lambda (i) (make-crack))
                (lambda (i) (+ i 1))
                0)))

(define (reset-cracks)
  (set! PRELOADED-CRACKS
        (append
         PRELOADED-CRACKS
         (map car EXISTING-CRACKS)))
  (set! EXISTING-CRACKS '()))

(define (crack x y)
  (if (not (null? PRELOADED-CRACKS))
      (let ((crack (car PRELOADED-CRACKS))
            (tail (cdr PRELOADED-CRACKS)))
        (set! PRELOADED-CRACKS tail)
        (set! EXISTING-CRACKS
              (cons (list crack x y)
                    EXISTING-CRACKS)))))

(define (make-crack)
  (define (random-scaled-float)
    (* (- (random-real) .5) 200.))

  (define (rotate x y degrees)
    (let* ((rad (* (/ degrees 180) PI))
           (rad-sin (sin rad))
           (rad-cos (cos rad)))
      (cons
       (- (* x rad-cos)
          (* y rad-sin))
       (+ (* x rad-sin)
          (* y rad-cos)))))
  
  (define (random-point)
    (cons (random-scaled-float)
          (random-scaled-float)))
  
  (define (random-directed-point p1 p2)
    (let* ((x (- (car p2) (car p1)))
           (y (- (cdr p2) (cdr p1)))
           (rotated (rotate x y (* (spread-number (random-real)) 45.))))
      (cons (+ (car p2) (car rotated))
            (+ (cdr p2) (cdr rotated)))))

  (define (make-line)
    (let ((first-point (random-point)))
      (let loop ((vertex-data (list 0. 0.
                                    (car first-point)
                                    (cdr first-point)))
                 (p1 (cons 0. 0.))
                 (p2 first-point)
                 (i (random-integer LINE-SPREAD)))
        (if (> i 0)
            (let ((point (random-directed-point p1 p2)))
              (loop (append (list (car p2) (cdr p2)
                                  (car point) (cdr point))
                            vertex-data)
                    p2
                    point
                    (- i 1)))
            vertex-data))))

  (let loop ((lines 0)
             (vertex-data '()))
    (if (< lines NUM-LINES)
        (loop (+ lines 1)
              (append (make-line)
                      vertex-data))
        (really-make-crack
         (make-crack-vertex-buffer vertex-data)
         (make-crack-color-buffer vertex-data)
         (/ (length vertex-data) 2)))))

(define (make-crack-vertex-buffer vertex-data)
  (vector->float-array
   (list->vector vertex-data)))

(define (make-crack-color-buffer vertex-data)
  (let ((num-vertex (/ (length vertex-data) 2)))
    (let loop ((acc '())
               (i 0))
      (if (< i num-vertex)
          (let ((color (list 255 255 255
                             (+ (random-integer 150) 30))))
            (loop (append color color acc)
                  (+ i 1)))
          (vector->unsigned-int8-array
           (list->vector acc))))))

(define (render-cracks)
  (for-each
   (lambda (crack-info)
     (apply render-crack crack-info))
   EXISTING-CRACKS))

(define (render-crack crack x y)
  (glLoadIdentity)

  (glVertexPointer 2 GL_FLOAT 0 (crack-vertex-buffer crack))
  (glEnableClientState GL_VERTEX_ARRAY)
  (glColorPointer 4 GL_UNSIGNED_BYTE 0 (crack-color-buffer crack))
  (glEnableClientState GL_COLOR_ARRAY)

  (let ((width (UIView-width (current-view)))
        (height (UIView-height (current-view))))
    (glTranslatef (/ x width)
                  (* (/ y height) 1.5)
                  0.)
    (glScalef (/ 1. width) (/ 1.5 height) 1.))

  (glDisable GL_CULL_FACE)
  (glDisable GL_DEPTH_TEST)
  (glDisable GL_LIGHTING)
  (glEnable GL_BLEND)
  (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
  (glLineWidth 1.)
  
  (glDrawArrays GL_LINES 0 (crack-num-vertices crack))
  (glDisable GL_BLEND)
  (glEnable GL_LIGHTING)
  (glDisableClientState GL_COLOR_ARRAY)
  (glEnable GL_DEPTH_TEST)
  (glEnable GL_CULL_FACE))

;; shatter

(define tri-pos
  (vector->float-array (vector -1. 0. 0.
                               0. 2. 0.
                               1. 0. 0.)))

(define (make-flying-triangle)
  (let ((pos (make-vec3d (* (spread-number (random-real)) 3.)
                         (* (spread-number (random-real)) 3.)
                         SCREEN-DEPTH))
        (rot (make-vec4d 0.
                         0.
                         1.
                         (* (random-real) 360.)))
        (rot-speed (* (random-real) 360.))
        (scale (make-vec3d (* (random-real) 2.)
                           (* (random-real) 2.)
                           (* (random-real) 2.)))
        (alpha 0.)
        (max-alpha (* (random-real) .7))
        (time (real-time)))
    (make-scene-object
     3d-perspective
     
     (lambda (this)
       (glLoadIdentity)
       (glTranslatef (vec3d-x pos)
                     (vec3d-y pos)
                     (vec3d-z pos))
       (glRotatef (vec4d-w rot)
                  (vec4d-x rot)
                  (vec4d-y rot)
                  (vec4d-z rot))
       (glScalef (vec3d-x scale)
                 (vec3d-y scale)
                 (vec3d-z scale))
       (glColor4f 1. 1. 1. alpha)
       (glDisable GL_LIGHTING)
       (glDisable GL_DEPTH_TEST)
       (glDisable GL_CULL_FACE)
       (glEnable GL_BLEND)
       (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
       (glVertexPointer 3 GL_FLOAT 0 tri-pos)
       (glDrawArrays GL_TRIANGLES 0 3)
       (glEnable GL_LIGHTING)
       (glEnable GL_DEPTH_TEST)
       (glEnable GL_CULL_FACE)
       (glDisable GL_BLEND))
     
     (lambda (self)
       (let* ((now (real-time))
              (change (- now time)))

         (set! alpha (min max-alpha
                          (+ alpha (* change 2.))))
         
         (vec3d-z-set!
          pos
          (- (vec3d-z pos)
             (* change 10.)))

         (vec4d-x-set!
          rot
          (- (vec4d-x rot)
             (* change 2.5)))
         
         (vec4d-y-set!
          rot
          (- (vec4d-y rot)
             (* change 2.5)))

         (vec4d-w-set!
          rot
          (- (vec4d-w rot)
             (* change rot-speed)))

         (set! time now))))))

(define (shatter)
  (let loop ((i 0))
    (if (< i 20)
        (begin
          (scene-list-add (make-flying-triangle))
          (loop (+ i 1))))))
