;;;; picking
;;; Handles animal picking from touch event

(define %%color-index 0)
(define %%color-map (make-vector 256 #f))

(define (get-next-color-index obj)
  ;; return a number in the range [1-255]
  (set! %%color-index (+ (remainder (+ %%color-index 1) 255) 1))
  (vector-set! %%color-map %%color-index obj)
  %%color-index)

(define (release-color-index index)
  (vector-set! %%color-map index #f))

(define (lookup-color-index index)
  (vector-ref %%color-map index))

(define (render-bounding-box obj)
  (let ((pos (scene-object-position obj))
        (rot (scene-object-rotation obj))
        (scale (scene-object-scale obj)))

    (glLoadIdentity)

    (if pos
        (glTranslatef (vec3d-x pos) (vec3d-y pos) (vec3d-z pos)))
    
    (if rot
        (glRotatef (vec4d-w rot)
                   (vec4d-x rot)
                   (vec4d-y rot)
                   (vec4d-z rot)))
    
    (if scale
        (glScalef (vec3d-x scale) (vec3d-y scale) (vec3d-z scale)))
    
    (glColor4f (exact->inexact (/ (scene-object-data obj) 255.)) 1. 1. 1.)
    
    (glVertexPointer
     3 GL_FLOAT 0
     (obj-bounding-box-mesh (scene-object-mesh obj)))
    (glEnableClientState GL_VERTEX_ARRAY)
    (glDisable GL_LIGHTING)
    (glDisable GL_FOG)
    (glDrawArrays GL_TRIANGLES 0 36)
    (if (equal? (current-level-name) "fog")
        (glEnable GL_FOG))))

(define %%intersection-queue '())

(define (queue-intersection x y)
  (set! %%intersection-queue
        (cons (list x y) %%intersection-queue)))

(define (dequeue-intersection)
  (if (null? %%intersection-queue)
      #f
      (let* ((lst (reverse %%intersection-queue))
             (res (car lst)))
        (set! %%intersection-queue (reverse (cdr lst)))
        res)))

(define (intersection-waiting?)
  (not (null? %%intersection-queue)))

(define (render-intersection-buffer)
  (glClearColor 0. 0. 0. 1.)
  (glClear (bitwise-ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
  
  (let* ((width (UIView-width (current-view)))
         (height (UIView-height (current-view)))
         (fov 40.)
         (aspect (/ width height)))
    (glMatrixMode GL_PROJECTION)
    (glLoadIdentity)
    (perspective fov aspect 1. 1000.)
    (lookat (make-vec3d 0. 0. 0.)
            (make-vec3d 0. 0. 1.)
            (make-vec3d 0. 1. 0.))
    (glMatrixMode GL_MODELVIEW)
    (glLoadIdentity))
  
  (run-render-queue
   (map (lambda (el)
          (lambda ()
            (render-bounding-box el)))
        scene-list)))

(define (handle-intersections)
  (define (bounds n x y)
    (max x (min n y)))

  (define (get-pixels x y)
    (let ((buf (make-unsigned-int8-array 324))
          (width (UIView-height (current-view)))
          (height (UIView-height (current-view))))
      (glReadPixels (bounds (- x 4) 0 width)
                    (bounds (- height y 4) 0 height)
                    9 9
                    GL_RGBA GL_UNSIGNED_BYTE
                    (->void-array buf))
      buf))

  (define (find-object pixels)
    (let loop ((i 0))
      (if (< i 324)
          (or (lookup-color-index (unsigned-int8-array-ref pixels i))
              (loop (+ i 4)))
          #f)))

  (define (find-object-at-point x y)
    (find-object (get-pixels x y)))
  
  (if (intersection-waiting?)
      (begin
        (render-intersection-buffer)
        (let loop ()
          (let ((loc (dequeue-intersection)))
            (if loc
                (begin
                  (and-let* ((obj (find-object-at-point (car loc) (cadr loc))))
                    (set! scene-list
                          (fold (lambda (el acc)
                                  (if (eq? el obj)
                                      (begin
                                        (on-entity-remove el)
                                        (on-entity-kill el)
                                        acc)
                                      (cons el acc)))
                                '()
                                scene-list)))
                  (loop))))))))
