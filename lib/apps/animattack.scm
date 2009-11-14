;;;; "app6"
;;; dynamic world

(declare (block)
         (standard-bindings)
         (extended-bindings))

(include "../util/srfi-2.scm")
(include "../events#.scm")
(include "../obj-loader2.scm")
(include "../scene.scm")
(include "../physics.scm")
(include "../util-3d.scm")
(include "../texture.scm")
(include "../intersection.scm")

;;; resources

(define cow-mesh (obj-load (resource "cow")))
(define sheep-mesh (obj-load (resource "sheep")))

;;; util

(define (quad v1 v2 v3 v4)
    (list v1 v2 v3 v1 v3 v4))

(define (vec3d-list->vector . args)
    (let ((v (make-vector (* (length args) 3))))
      (let loop ((tail args)
                 (i 0))
        (if (null? tail)
            v
            (let ((vec (car tail)))
              (vector-set! v (* i 3) (vec3d-x vec))
              (vector-set! v (+ (* i 3) 1) (vec3d-y vec))
              (vector-set! v (+ (* i 3) 2) (vec3d-z vec))
              (loop (cdr tail) (+ i 1)))))))

;;; controls

(define yaw 0.)
(define pitch 0.)

(define (reset-camera)
  (glLoadIdentity)
  (glRotatef (- (exact->inexact yaw)) 1. 0. 0.)
  (glRotatef (exact->inexact pitch) 0. 1. 0.))

(define %%touch-coords (make-table))

(define (record-touch touch)
  (table-set! %%touch-coords touch (UITouch-location touch)))

(define (update-touch/get-movement touch)
  (let* ((old-loc (table-ref %%touch-coords touch))
         (loc (UITouch-location touch))
         (x (- (car old-loc) (car loc)))
         (y (- (cdr old-loc) (cdr loc))))
    (record-touch touch)
    (cons x y)))

(define (remove-touch touch)
  (table-set! %%touch-coords touch))

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
                                        acc)
                                      (cons el acc)))
                                '()
                                scene-list)))
                  (loop))))))))

(define-event-handler (touches-began touches event)
  (for-each (lambda (el)
              (let ((loc (UITouch-location el)))
                (queue-intersection (car loc) (cdr loc))
                (make-laser loc)))
            touches))

;;; util

(define (spread-number fl)
  (- (* fl 2.) 1.))

;;; glass cracking

(define %%crack-points '())
(define %%cracks '())
(define %%crack-dirty #f)

(define (deviate n)
  (+ n (* (- (random-real) .5) 250.)))

(define (add-point point)
  (set! %%crack-points
        (cons point %%crack-points)))

(define-type crack
  point1
  point2
  color
  alpha)

(define (add-crack point1 point2)
  (set! %%cracks
        (cons (make-crack point1
                          point2
                          (+ (random-integer 100) 155)
                          (+ (random-integer 80) 100))
              %%cracks)))

(define-type point/distance
  point
  distance)

(define (get-point-distance point1 point2)
  (vec2d-length (make-vec2d (- (car point1) (car point2))
                            (- (cdr point1) (cdr point2)))))

(define (find-closest-points point1 x)
  (let ((data (sort-list (map (lambda (point2)
                                (make-point/distance
                                 point2
                                 (get-point-distance point1 point2)))
                              %%crack-points)
                         (lambda (p/dist1 p/dist2)
                           (< (point/distance-distance p/dist1)
                              (point/distance-distance p/dist2))))))
    (map point/distance-point
         (take data x))))

(define (crack x y)
  (define (add-random-crack point)
    (let ((new-point (cons (deviate (car point))
                           (deviate (cdr point)))))
      (add-point new-point)
      (add-crack point new-point)))
  
  (let ((point (cons x y)))
    (if (not (null? %%cracks))
        (let ((points (find-closest-points point
                                           (+ (random-integer 3) 1))))
          (if (= (random-integer 2) 0)
              ;; Only add partial cracks
              (for-each (lambda (point2)
                          (let ((p1 (make-vec2d (car point) (cdr point)))
                                (p2 (make-vec2d (car point2) (cdr point2))))
                            (let* ((p3 (vec2d-add
                                            p2
                                            (vec2d-scalar-mul (vec2d-sub p1 p2)
                                                              (+ (* (random-real) .5) .5))))
                                   (point3 (cons (vec2d-x p3)
                                                 (vec2d-y p3))))
                              (add-point point3)
                              (add-crack point2 point3))))
                        points)
              ;; Add completely new cracks
              (begin
                (add-point point)
                (for-each (lambda (point2)
                            (add-crack point2 point))
                          points)
                (let ((num-new-cracks (random-integer 3)))
                  (unfold (lambda (i) (>= i num-new-cracks))
                          (lambda (i) (add-random-crack point))
                          (lambda (i) (+ i 1))
                          0)))))
        (begin
          (add-point point)
          (add-random-crack point)
          (add-random-crack point)
          (add-random-crack point))))
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

(define (crack-to-color crack)
  (let ((health-inv (- 1. (get-health))))
    (apply
     append
     (unfold (lambda (i) (>= i 6))
             (lambda (i) (list 200 200 200 (crack-alpha crack)))
             (lambda (i) (+ i 1))
             0))))

(define (cache-cracks)
  (let* ((vertices (apply
                    vec3d-list->vector
                    (fold (lambda (crack acc)
                            (append (crack-to-polygon crack)
                                    acc))
                          '()
                          %%cracks)))
         (colors (list->vector
                  (fold (lambda (crack acc)
                          (append (crack-to-color crack)
                                  acc))
                        '()
                        %%cracks))))
    (set! %%crack-vertices (vector->float-array vertices))
    (set! %%crack-colors (vector->unsigned-int8-array colors))
    (set! %%crack-vertices-length (* (length %%cracks) 6))
    (set! %%crack-dirty #f)))

(define (render-cracks)
  (if %%crack-dirty
      (cache-cracks))
  
  (glLoadIdentity)
  
  (if %%crack-vertices
      (begin
        (glVertexPointer 3 GL_FLOAT 0 %%crack-vertices)
        (glEnableClientState GL_VERTEX_ARRAY)
        (glColorPointer 4 GL_UNSIGNED_BYTE 0 %%crack-colors)
        (glEnableClientState GL_COLOR_ARRAY)

        (glDisable GL_CULL_FACE)
        (glDisable GL_DEPTH_TEST)
        (glDisable GL_LIGHTING)
        (glEnable GL_BLEND)
        (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
        (glDrawArrays GL_TRIANGLES 0 %%crack-vertices-length)
        (glDisable GL_BLEND)
        (glEnable GL_LIGHTING)
        (glDisableClientState GL_COLOR_ARRAY)
        (glEnable GL_DEPTH_TEST)
        (glEnable GL_CULL_FACE))))

;;; mouse picking

;; (define (find-entity-intersection x y z)
;;   (let loop ((tail %%entities))
;;     (if (null? tail)
;;         #f
;;         (let* ((obj (car tail))
;;                (mesh (scene-object-mesh obj))
;;                (bb (obj-bounding-box mesh)))
;;           (if (ray-box-intersection (bounding-box-min-x bb)
;;                                     (bounding-box-min-y bb)
;;                                     (bounding-box-min-z bb)
;;                                     (bounding-box-max-x bb)
;;                                     (bounding-box-max-y bb)
;;                                     (bounding-box-max-z bb)
;;                                     0. 0. 0.
;;                                     x y z)
;;               obj
;;               (loop (cdr tail)))))))

(define %%color-index 0)
(define %%color-map (make-vector 256 #f))

(define (get-next-color-index obj)
  ;; return a number in the range [1-255]
  (set! %%color-index (+ (remainder (+ %%color-index 1) 255) 1))
  (vector-set! %%color-map %%color-index obj)
  %%color-index)

(define (lookup-color-index index)
  (vector-ref %%color-map index))

(define (render-bounding-box obj)
  (let ((pos (scene-object-position obj))
        (rot (scene-object-rotation obj))
        (bbox (obj-bounding-box (scene-object-mesh obj))))

    (define-macro (bb attr)
      `(,(string->symbol
          (string-append "bounding-box-" (symbol->string attr)))
        bbox))

    (glLoadIdentity)
    
    (if pos
        (glTranslatef (vec3d-x pos) (vec3d-y pos) (vec3d-z pos)))  
    (if rot
        (glRotatef (vec4d-w rot)
                   (vec4d-x rot)
                   (vec4d-y rot)
                   (vec4d-z rot)))

    (glColor4f (exact->inexact (/ (scene-object-data obj) 255.)) 1. 1. 1.)
    
    (glVertexPointer
     3 GL_FLOAT 0
     (vector->float-array
      (apply vec3d-list->vector
             (append
              ;; Create counter-clockwise quads for each side of the
              ;; bounding box
              
              ;; min-x plane
              (quad (make-vec3d (bb min-x) (bb min-y) (bb min-z))
                    (make-vec3d (bb min-x) (bb min-y) (bb max-z))
                    (make-vec3d (bb min-x) (bb max-y) (bb max-z))
                    (make-vec3d (bb min-x) (bb max-y) (bb min-z)))

              ;; max-x plane
              (quad (make-vec3d (bb max-x) (bb min-y) (bb min-z))
                    (make-vec3d (bb max-x) (bb max-y) (bb min-z))
                    (make-vec3d (bb max-x) (bb max-y) (bb max-z))
                    (make-vec3d (bb max-x) (bb min-y) (bb max-z)))

              ;; min-z plane
              (quad (make-vec3d (bb min-x) (bb min-y) (bb min-z))
                    (make-vec3d (bb min-x) (bb max-y) (bb min-z))
                    (make-vec3d (bb max-x) (bb max-y) (bb min-z))
                    (make-vec3d (bb max-x) (bb min-y) (bb min-z)))

              ;; max-z plane
              (quad (make-vec3d (bb min-x) (bb min-y) (bb max-z))
                    (make-vec3d (bb max-x) (bb min-y) (bb max-z))                    
                    (make-vec3d (bb max-x) (bb max-y) (bb max-z))
                    (make-vec3d (bb min-x) (bb max-y) (bb max-z)))

              ;; min-y plane
              (quad (make-vec3d (bb min-x) (bb min-y) (bb min-z))
                    (make-vec3d (bb max-x) (bb min-y) (bb min-z))
                    (make-vec3d (bb max-x) (bb min-y) (bb max-z))
                    (make-vec3d (bb min-x) (bb min-y) (bb max-z)))

              ;; max-y plane
              (quad (make-vec3d (bb min-x) (bb max-y) (bb min-z))
                    (make-vec3d (bb min-x) (bb max-y) (bb max-z))
                    (make-vec3d (bb max-x) (bb max-y) (bb max-z))
                    (make-vec3d (bb max-x) (bb max-y) (bb min-z)))))))
    (glEnableClientState GL_VERTEX_ARRAY)
    (glDisable GL_LIGHTING)
    (glDrawArrays GL_TRIANGLES 0 36)
    (glEnable GL_LIGHTING)))

;;; app

;; x, y, and z should be in world coords relative to the camera
(define (unproject x y z)
  (let* ((screen-x (/ x z))
         (screen-y (/ y z))
         (width (UIView-width (current-view)))
         (height (UIView-height (current-view)))
         (pers (current-perspective))
         (x-max (perspective-xmax pers))
         (y-max (perspective-ymax pers))
         (x-min (perspective-xmin pers))
         (y-min (perspective-ymin pers))
         (x-width (- x-max x-min))
         (y-width (- y-max y-min)))
    (list
     (* (/ (- x-max screen-x) x-width)
        width)
     (* (/ (- y-max screen-y) y-width)
        height))))

;; x and y should be in screen coords [0, width) and [0, height)
(define (project x y #!optional (depth 1.))
  (let* ((width (UIView-width (current-view)))
         (height (UIView-height (current-view)))
         (pers (current-perspective))
         (x-max (perspective-xmax pers))
         (y-max (perspective-ymax pers))
         (x-min (perspective-xmin pers))
         (y-min (perspective-ymin pers))
         (x-width (- x-max x-min))
         (y-width (- y-max y-min)))
    (list
     (* (- x-max (* (/ x width) x-width)) depth)
     (* (- y-max (* (/ y height) y-width)) depth)
     depth)))

(define (update-audio obj)
  (define (saturate n)
    (min 1. (max 0. n)))

  (let ((source (scene-object-voice-source obj))
        (pos (scene-object-position obj)))
    #t
    #;
    (alSourcef source
               AL_GAIN
               (- 1. (saturate
                      (/ (- (vec3d-z pos) %%screen-depth) 40.))))))

(define (global-update el)
  (update-physics el)
  (update-audio el)
  (let ((pos (scene-object-position el)))
    (if (< (vec3d-z pos) %%screen-depth)
        (begin
          (impact el)
          (if (not (no-more-life?))
              (begin
                (vec3d-z-set! pos %%screen-depth)
                (apply crack
                       (unproject (vec3d-x pos) (vec3d-y pos) (vec3d-z pos)))
                (play-thud-for-entity el)
                (focus-life-meter)
                (scene-object-velocity-set! el (make-vec3d 0. 0. 0.))
                (scene-object-acceleration-set! el (make-vec3d 0. -10. 0.))))))))

(define (%%get-random-time)
  (+ (real-time) (* (random-real) 2.5)))

(define (%%get-random-mesh)
  (let ((x (random-integer 100)))
    (cond
     ((< x 50) cow-mesh)
     ((>= x 50) sheep-mesh))))

(define %%next-time (%%get-random-time))
(define %%entities '())

(define (possibly-make-entity)
  (if (> (real-time) %%next-time)
      (let ((entity (make-entity)))
        (scene-list-add entity)
        (set! %%entities (cons entity %%entities))
        (set! %%next-time (%%get-random-time)))))

(define %%entity-max-depth 40.)

(define (make-entity)
  (let* ((pos (make-vec3d
               (* (spread-number (random-real)) 7.) -28. %%entity-max-depth))
         (to-eye (vec3d-unit (vec3d-sub (make-vec3d 0. 0. 0.)
                                        pos)))
         (x (* (spread-number (random-real)) 3.16))
         (thrust (+ 15. (* x (abs x))))
         (vel (make-vec3d (* (vec3d-x to-eye) thrust)
                          (+ 25.5 (spread-number (random-real)))
                          (* (vec3d-z to-eye) thrust))))
    (let ((obj (make-scene-object
                (%%get-random-mesh)
                #f
                pos
                (make-vec4d (random-real)
                            (random-real)
                            0.
                            230.)
                1.5
                vel
                #f
                (let ((speed (* (random-real) 4.)))
                  (lambda (this)
                    (scene-object-rotation-set!
                     this
                     (vec4d-add (scene-object-rotation this)
                                (make-vec4d 0. 0. 0. speed)))
                    (let* ((pos (scene-object-position this))
                           (screen-y (cadr (unproject (vec3d-x pos)
                                                      (vec3d-y pos)
                                                      (vec3d-z pos))))
                           (screen-height (UIView-height (current-view))))
                      (if (> screen-y (+ screen-height 100))
                          (begin
                            (on-entity-remove this)
                            #f)
                          this)))))))
      (play-voice-for-entity obj)
      (scene-object-data-set! obj (get-next-color-index obj))
      obj)))

(define (play-voice-for-entity obj)
  (let* ((mesh (scene-object-mesh obj))
         (buffer
          (cond
           ((eq? cow-mesh mesh) moo-audio)
           ((eq? sheep-mesh mesh) bah-audio)))
         (source (make-audio-source buffer)))
    (play-audio source)
    (scene-object-voice-source-set! obj source)))

(define (play-thud-for-entity obj)
  (let ((source (make-audio-source thud-audio)))
    (play-audio source)
    (scene-object-thud-source-set! obj source)))

(define (on-entity-remove obj)
  (let ((voice-source (scene-object-voice-source obj))
        (thud-source (scene-object-thud-source obj)))
    (if thud-source
        (begin
          (stop-audio thud-source)
          (free-audio-source thud-source)))
    
    (stop-audio voice-source)
    (free-audio-source voice-source))
  (scene-object-voice-source-set! obj #f)
  (scene-object-thud-source-set! obj #f))

;; life

(define %%entity-forces
  `((,cow-mesh 2)
    (,sheep-mesh 1)))

(define (entity-force el)
  (let ((mesh (scene-object-mesh el)))
    (and-let* ((x (assq mesh %%entity-forces)))
      (cadr x))))

(define %%max-life 30)
(define %%life %%max-life)

(define (impact el)
  (let ((f (entity-force el)))
    (set! %%life (- %%life f))))

(define (no-more-life?)
  (<= %%life 0))

(define %%life-mesh
  (vector->float-array (vector .0 .95
                               .0 1.
                               1. .95
                               1. 1.)))

(define %%render-life-time 0.)
(define %%render-life-length .7)

(define (render-life)
  (let ((age (- (real-time) %%render-life-time))
        (health (exact->inexact (/ %%life %%max-life))))
    (glLoadIdentity)
    (glDisable GL_DEPTH_TEST)
    (glDisable GL_LIGHTING)
    (glVertexPointer 2 GL_FLOAT 0 %%life-mesh)
    (glEnableClientState GL_VERTEX_ARRAY)
    (glEnable GL_BLEND)
    (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
    (let ((health-inv (- 1. health)))
      (glColor4f health-inv
                 (* health-inv .25)
                 (* health-inv .25)
                 (max (- 1. (/ age %%render-life-length)) .25)))

    ;; Scale according the amount of life left (the height will indicate
    ;; it: full height is full life)
    (glScalef health 1. 1.)
    
    (glDrawArrays GL_TRIANGLE_STRIP 0 4)

    (glDisable GL_BLEND)
    (glEnable GL_LIGHTING)
    (glEnable GL_DEPTH_TEST)))

(define (focus-life-meter)
  (set! %%render-life-time (real-time)))

(define (get-health)
  (/ %%life %%max-life))

(define %%shatter-source #f)
(define (on-death)
  (if (not %%shatter-source)
      (let ((source (make-audio-source shatter-audio)))
        (play-audio source)
        (set! %%shatter-source source))))

;; lasers

(define %%lasers '())

(define (add-laser dg point color lifetime)
  (let ((then (real-time))
        (explosion-source (make-audio-source explosion-audio)))
    (alSourcef explosion-source AL_GAIN .5)
    ;; Play the explosion sound, and free it
    (play-audio explosion-source)
    ;; Ewwww, look at that ugly hack!
    (thread-start!
     (make-thread
      (lambda ()
        (thread-sleep! 1.3)
        (free-audio-source explosion-source))))

    (set!
     %%lasers
     (cons 
      (lambda ()
        (let ((now (real-time))
              (width (UIView-width (current-view)))
              (height (UIView-height (current-view))))
          (if (>= (- now then) lifetime)
              #f
              (let ((pers (current-perspective)))
                (glLoadIdentity)
                (glTranslatef (exact->inexact (* (/ (car point) width) (perspective-xmax pers)))
                              (exact->inexact (* (/ (cdr point) height) (perspective-ymin pers)))
                              0.)
                (glRotatef dg 0. 0. 1.)
                (glTranslatef 0. -.05 0.)
                (glScalef 1.5 .1 1.)

                ;; Freaking alpha-premultiplication that Cocoa does
                ;; automatically. This simply allows me to fade out a
                ;; textured polygon, which is broken due to Apple's
                ;; crappy premultiplication which is forced on you.
                ;; Basically, I premultiply the fade out into the
                ;; color values using glColor4f since the texture
                ;; environment is set to GL_MODULATE.
                (glEnable GL_BLEND)
                (glBlendFunc GL_ONE GL_ONE_MINUS_SRC_ALPHA)
                (let ((fade (- 1. (/ (- now then)
                                     lifetime))))
                  (glColor4f (* fade (vec3d-x color))
                             (* fade (vec3d-y color))
                             (* fade (vec3d-z color))
                             fade))
                
                (image-render (case (random-integer 2)
                                ((0) line-texture)
                                ((1) line2-texture)))
                
                (glDisable GL_BLEND)
                #t))))
      %%lasers))))

(define (make-laser point)
  (define (c)
    (make-vec3d 1. 1. 1.))

  (define (l)
    (* (random-real) .4))
  
  (add-laser 0. point (c) (l))
  (add-laser 90. point (c) (l))
  (add-laser 180. point (c) (l))
  (add-laser 270. point (c) (l))
  (add-laser (* (random-real) 15.)
             point
             (c)
             (l))
  (add-laser (+ 90. (* (spread-number (random-real)) 15.))
             point
             (c)
             (l))
  (add-laser (+ 180. (* (spread-number (random-real)) 15.))
             point
             (c)
             (l))
  (add-laser (+ 270. (* (spread-number (random-real)) 15.))
             point
             (c)
             (l)))

(define (render-lasers)
  (set! %%lasers
        (reverse
         (fold (lambda (render-laser acc)
                 (let ((result (render-laser)))
                   (if result
                       (cons render-laser acc)
                       acc)))
               '()
               %%lasers))))

;; engine

(define background-texture #f)
(define line-texture #f)
(define line2-texture #f)
(define bah-audio #f)
(define moo-audio #f)
(define thud-audio #f)
(define shatter-audio #f)
(define explosion-audio #f)

(define (init)
  (random-source-randomize! default-random-source)

  (glEnable GL_DEPTH_TEST)
  (glEnable GL_CULL_FACE)
  (glCullFace GL_BACK)
  (glShadeModel GL_SMOOTH)
  (glEnable GL_RESCALE_NORMAL)

  ;; Set up the lighting. We want it pretty lit up, but some shading
  ;; to show form. Provide backlighting for ambience, and also cast
  ;; light forward to really brighten it up. Both directional with
  ;; lights with white ambience.
  (glEnable GL_LIGHTING)
  (glEnable GL_LIGHT0)
  (glLightfv GL_LIGHT0 GL_AMBIENT (vector->float-array (vector 1. 1. 1. 1.)))
  (glLightfv GL_LIGHT0 GL_POSITION (vector->float-array (vector 0. 0. -1. 0.)))
  (glLightfv GL_LIGHT0 GL_DIFFUSE (vector->float-array (vector 1. 1. 1. 1.)))
  (glLightfv GL_LIGHT1 GL_AMBIENT (vector->float-array (vector 1. 1. 1. 1.)))
  (glLightfv GL_LIGHT1 GL_POSITION (vector->float-array (vector 0. 0. 1. 0.)))
  (glLightfv GL_LIGHT1 GL_DIFFUSE (vector->float-array (vector 1. 1. 1. 1.)))
  (glLightModelfv GL_LIGHT_MODEL_AMBIENT (vector->float-array (vector .3 .3 .3 1.)))

  ;; This value is rather arbitrary; it just depends on how heavy we
  ;; set everything in the scene. We are using a gravity with an
  ;; acceleration of 11 m/s as a reference point.
  (set! GRAVITY (make-vec3d 0. -11. 0.))

  (let ((image (CGImageRef-load "sky.png")))
    (set! background-texture (image-opengl-upload
                              (CGImageRef-data image)
                              (CGImageRef-width image)
                              (CGImageRef-height image))))

  (let ((line (CGImageRef-load "line.png"))
        (line2 (CGImageRef-load "line2.png")))
    (set! line-texture (image-opengl-upload
                        (CGImageRef-data line)
                        (CGImageRef-width line)
                        (CGImageRef-height line)))
    (set! line2-texture (image-opengl-upload
                         (CGImageRef-data line2)
                         (CGImageRef-width line2)
                         (CGImageRef-height line2))))

  (init-audio)
  (set! bah-audio (load-audio "bah.wav"))
  (set! moo-audio (load-audio "moo.wav"))
  (set! thud-audio (load-audio "thud.wav"))
  (set! shatter-audio (load-audio "shatter.wav"))
  (set! explosion-audio (load-audio "explosion2.wav")))

(define (run)
  (possibly-make-entity)
  (scene-list-update global-update))

(define (render)
  (handle-intersections)

  (glClearColor 0. 0. 0. 1.)
  (glClear (bitwise-ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))

  (let ((width (UIView-width (current-view)))
        (height (UIView-height (current-view))))
    
    ;; Pre 2d
    (glMatrixMode GL_PROJECTION)
    (glLoadIdentity)
    (glOrthof 0.0 1.0 1.0 0.0 -1.0 1.0)
    (glMatrixMode GL_MODELVIEW)
    (glLoadIdentity)
    (glColor4f 1. 1. 1. 1.)
    (image-render background-texture)

    ;; 3d
    (let* ((fov 40.)
           (aspect (/ width height)))
      (glMatrixMode GL_PROJECTION)
      (glLoadIdentity)
      (perspective fov aspect 1. 1000.)
      (lookat (make-vec3d 0. 0. 0.)
              (make-vec3d 0. 0. 1.)
              (make-vec3d 0. 1. 0.))
      (glMatrixMode GL_MODELVIEW)
      (glLoadIdentity))

    (run)
    (run-render-queue (scene-list->render-queue))
    
    (if (not (no-more-life?))
        (render-cracks)
        (on-death))

    ;; Post 2d
    (glMatrixMode GL_PROJECTION)
    (glLoadIdentity)
    (ortho 0.0 1.0 1.0 0.0 -1.0 1.0)
    (glMatrixMode GL_MODELVIEW)
    (render-life)

    (glMatrixMode GL_PROJECTION)
    (glLoadIdentity)
    (ortho 0.0 1.0 1.5 0.0 -1.0 1.0)
    (glMatrixMode GL_MODELVIEW)
    (render-lasers))
  (##gc))

(define (get-title)
  "")
