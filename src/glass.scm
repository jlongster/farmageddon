;;;; glass simulation
;;; Simulates a piece of glass which cracks 

(define (deviate n)
  (+ n (* (- (random-real) .5) 250.)))

(define-type crack
  point1
  point2
  color
  alpha)

(define %%cracks '())
(define %%crack-dirty #f)

(define (add-crack point1 point2)
  (set! %%cracks
        (cons (make-crack point1
                          point2
                          (+ (random-integer 100) 155)
                          (+ (random-integer 120) 100))
              %%cracks)))

(define (crack x y)
  (define (rotate x y degrees)
    (let* ((rad (* (/ degrees 180) PI))
           (rad-sin (sin rad))
           (rad-cos (cos rad)))
      (cons
       (- (* x rad-cos)
          (* y rad-sin))
       (+ (* x rad-sin)
          (* y rad-cos)))))
  
  (define (random-closeby-point point)
    (cons (deviate (car point))
          (deviate (cdr point))))

  (define (random-directed-point p1 p2)
    (let* ((x (- (car p2) (car p1)))
           (y (- (cdr p2) (cdr p1)))
           (rotated (rotate x y (* (spread-number (random-real)) 45.))))
      (cons (+ (car p2) (car rotated))
            (+ (cdr p2) (cdr rotated)))))

  (define (add-cracks-from-point p1)
    (let ((p2 (random-closeby-point p1)))
      (add-crack p1 p2)
    
      (let loop ((p1 p1)
                 (p2 p2)
                 (i (random-integer 3)))
        (if (> i 0)
            (let ((point (random-directed-point p1 p2)))
              (add-crack p2 point)
              (loop p2 point (- i 1)))))))

  (let ((point (cons x y)))
    (let loop ((i 0))
      (if (< i 35)
          (begin
            (add-cracks-from-point point)
            (loop (+ i 1))))))

  (set! %%crack-dirty #t))

(define %%crack-vertices #f)
(define %%crack-colors #f)
(define %%crack-vertices-length #f)

(define %%screen-depth 10.)
(define %%screen-width .2)

(define (crack-to-polygon crack)
  (let ((x1 (car (crack-point1 crack)))
        (y1 (cdr (crack-point1 crack)))
        (x2 (car (crack-point2 crack)))
        (y2 (cdr (crack-point2 crack))))
    (let ((p1 (apply make-vec3d (project x1 y1 10.)))
          (p2 (apply make-vec3d (project x2 y2 10.))))
      (quad p1 p2
            (make-vec3d (vec3d-x p2)
                        (+ (vec3d-y p2) 0.1)
                        (+ (vec3d-z p2) %%screen-width))
            (make-vec3d (vec3d-x p1)
                        (+ (vec3d-y p1) 0.1)
                        (+ (vec3d-z p1) %%screen-width))))))

(define (crack-to-lines crack)
  (let ((width (UIView-width (current-view)))
        (height (UIView-height (current-view)))
        (x1 (car (crack-point1 crack)))
        (y1 (cdr (crack-point1 crack)))
        (x2 (car (crack-point2 crack)))
        (y2 (cdr (crack-point2 crack))))
    (list (/ (exact->inexact x1) width)
          (/ (exact->inexact y1) height)
          (/ (exact->inexact x2) width)
          (/ (exact->inexact y2) height))))

(define (crack-to-color crack num-verts)
  (let ((health-inv (- 1. (get-health))))
    (apply
     append
     (unfold (lambda (i) (>= i num-verts))
             (lambda (i) (list 200 200 255 (crack-alpha crack)))
             (lambda (i) (+ i 1))
             0))))

(define (cache-cracks)
  (let* ((vertices (list->vector
                    (fold (lambda (crack acc)
                            (append (crack-to-lines crack)
                                    acc))
                          '()
                          %%cracks)))
         (colors (list->vector
                  (fold (lambda (crack acc)
                          (append (crack-to-color crack 4)
                                  acc))
                        '()
                        %%cracks))))
    (set! %%crack-vertices (vector->float-array vertices))
    (set! %%crack-colors (vector->unsigned-int8-array colors))
    (set! %%crack-vertices-length (* (length %%cracks) 2))
    (set! %%crack-dirty #f)))

(define (render-cracks)
  (if %%crack-dirty
      (cache-cracks))
  
  (glLoadIdentity)
  
  (if %%crack-vertices
      (begin
        (glVertexPointer 2 GL_FLOAT 0 %%crack-vertices)
        (glEnableClientState GL_VERTEX_ARRAY)
        (glColorPointer 4 GL_UNSIGNED_BYTE 0 %%crack-colors)
        (glEnableClientState GL_COLOR_ARRAY)

        (glDisable GL_CULL_FACE)
        (glDisable GL_DEPTH_TEST)
        (glDisable GL_LIGHTING)
        (glEnable GL_BLEND)
        (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
        (glLineWidth 2.)
        (glDrawArrays GL_LINES 0 %%crack-vertices-length)
        (glDisable GL_BLEND)
        (glEnable GL_LIGHTING)
        (glDisableClientState GL_COLOR_ARRAY)
        (glEnable GL_DEPTH_TEST)
        (glEnable GL_CULL_FACE))))
