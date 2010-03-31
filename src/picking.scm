;;;; picking
;;; Handles animal picking from touch event

(declare (block)
         (standard-bindings)
         (extended-bindings))

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
  (let ((pos (mesh-object-position obj))
        (rot (mesh-object-rotation obj))
        (scale (mesh-object-scale obj)))

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
    
    (glColor4f (exact->inexact (/ (mesh-object-data obj) 255.)) 1. 1. 1.)
    
    (glVertexPointer
     3 GL_FLOAT 0
     (obj-bounding-box-mesh (mesh-object-mesh obj)))
    (glEnableClientState GL_VERTEX_ARRAY)
    (glDisable GL_LIGHTING)
    (glDrawArrays GL_TRIANGLES 0 36)
    (glEnable GL_LIGHTING)))

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

  (load-perspective 3d-perspective)
  
  (scene-list-render
   (lambda (obj)
     (if (mesh-object? obj)
         (render-bounding-box obj)))))

(define (handle-intersections)
  (define (bounds n x y)
    (max x (min n y)))

  (define (get-pixels x y)
    ;; buf should be the size of width*height*4
    (let ((buf (make-unsigned-int8-array 1))
          (width (UIView-height (current-view)))
          (height (UIView-height (current-view))))
      (glReadPixels (bounds (- x 0) 0 width)
                    (bounds (- height y 0) 0 height)
                    1 1
                    GL_RGBA GL_UNSIGNED_BYTE
                    (->void-array buf))
      buf))

  (define (find-object pixels)
    (let loop ((i 0))
      (if (< i 1)
          (or (lookup-color-index (unsigned-int8-array-ref pixels i))
              (loop (+ i 4)))
          #f)))

  (define (find-object-at-point x y)
    (let* ((buf (get-pixels x y))
           (obj (find-object buf)))
      (free buf)
      obj))

  (if (intersection-waiting?)
      (begin
        (render-intersection-buffer)
        (let loop ()
          (let ((loc (dequeue-intersection)))
            (if loc
                (begin
                  (and-let* ((obj (find-object-at-point (car loc) (cadr loc))))
                    (on-entity-kill obj)
                    (add-dust (car loc)
                              (cadr loc)
                              (vec3d-z (mesh-object-position obj)))
                    (remove-entity obj))
                  (loop))))))))
