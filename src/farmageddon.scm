
(declare (block)
         (standard-bindings)
         (extended-bindings))

;; libraries

(include "lib/srfi/srfi-1.scm")
(include "lib/srfi/srfi-2.scm")
(include "lib/vectors.scm")
(include "lib/events#.scm")
(include "lib/events.scm")
(include "lib/obj-loader.scm")
(include "lib/scene.scm")
(include "lib/physics.scm")
(include "lib/standard-meshes.scm")
(include "lib/texture.scm")

;; application components

(include "lasers.scm")

;;; resources

(define chicken-mesh (obj-load (resource "chicken") #t))
(define duck-mesh (obj-load (resource "duck") #t))
(define cow-mesh (obj-load (resource "cow") #t))
(define sheep-mesh (obj-load (resource "sheep") #t))
 
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
                                        (on-entity-kill el)
                                        acc)
                                      (cons el acc)))
                                '()
                                scene-list)))
                  (loop))))))))

(define-event-handler (touches-began touches event)
  (for-each (lambda (el)
              (let ((loc (UITouch-location el)))
                (queue-intersection (car loc) (cdr loc))
                (make-laser loc)
                ))
            touches))

;;; util

(define (spread-number fl)
  (- (* fl 2.) 1.))

;;; glass cracking

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
  (define (rotate point degrees)
    (let* ((rad (* (/ degrees 180) PI))
           (rad-sin (sin rad))
           (rad-cos (cos rad)))
      (make-vec2d
       (- (* (vec2d-x point) rad-cos)
          (* (vec2d-y point) rad-sin))
       (+ (* (vec2d-x point) rad-sin)
          (* (vec2d-y point) rad-cos)))))
  
  (define (random-closeby-point point)
    (cons (deviate (car point))
          (deviate (cdr point))))

  (define (random-directed-point p1 p2)
    (let ((p1 (make-vec2d (car p1) (cdr p1)))
          (p2 (make-vec2d (car p2) (cdr p2))))
      (let* ((vec1 (vec2d-sub p2 p1))
             (vec2 (rotate vec1 (* (spread-number (random-real)) 45.)))
             (point (vec2d-add p2 vec2)))
        (cons (vec2d-x point)
              (vec2d-y point)))))

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
        (rot (scene-object-rotation obj)))

    (glLoadIdentity)

    (if pos
        (glTranslatef (vec3d-x pos) (vec3d-y pos) (vec3d-z pos)))  
    (if rot
        (glRotatef (vec4d-w rot)
                   (vec4d-x rot)
                   (vec4d-y rot)
                   (vec4d-z rot)))
    (glScalef ENTITY_SCALE ENTITY_SCALE ENTITY_SCALE)
    
    (glColor4f (exact->inexact (/ (scene-object-data obj) 255.)) 1. 1. 1.)
    
    (glVertexPointer
     3 GL_FLOAT 0
     (obj-bounding-box-mesh (scene-object-mesh obj)))
    (glEnableClientState GL_VERTEX_ARRAY)
    (glDisable GL_LIGHTING)
    (glDisable GL_FOG)
    (glDrawArrays GL_TRIANGLES 0 36)
    (if (equal? (level-name (current-level)) "fog")
        (glEnable GL_FOG))))

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
                (scene-object-velocity-set! el (make-vec3d 0. 0. 0.))
                (scene-object-acceleration-set! el (make-vec3d 0. -10. 0.))))))))

(define (%%get-random-time)
  (+ (real-time) (* (random-real) LVL_FREQUENCY)))

(define %%next-time #f)
(define %%entities '())

(define (possibly-make-entity)
  (if %%next-time
      (if (> (real-time) %%next-time)
          (let ((entity (make-entity)))
            (scene-list-add entity)
            (set! %%entities (cons entity %%entities))
            (set! %%next-time (%%get-random-time))))
      (set! %%next-time (%%get-random-time))))

(define %%entity-max-depth 40.)

(define ENTITY_SCALE 4.)

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
                (LVL_GET_MESH)
                #f
                pos
                (make-vec4d (random-real)
                            (random-real)
                            0.
                            230.)
                ENTITY_SCALE
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
           ((eq? sheep-mesh mesh) bah-audio)
           ((eq? chicken-mesh mesh) chicken-audio)
           (else #f))))
    (if buffer
        (let ((source (make-audio-source buffer)))
          (play-audio source)
          (scene-object-voice-source-set! obj source)))))

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

    (if voice-source
        (begin
          (stop-audio voice-source)
          (free-audio-source voice-source))))
  (scene-object-voice-source-set! obj #f)
  (scene-object-thud-source-set! obj #f))

(define (on-entity-kill obj)
  (update-score obj))

;; life

(define %%entity-forces
  `((,cow-mesh 2)
    (,sheep-mesh 1)
    (,duck-mesh 1)
    (,chicken-mesh .5)))

(define (entity-force el)
  (let ((mesh (scene-object-mesh el)))
    (and-let* ((x (assq mesh %%entity-forces)))
      (cadr x))))

(define %%max-life 15000)
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

;;(define )

(define %%life-groove #f)
(define %%life-bar #f)

(define (life-init)
  (let ((image (CGImageRef-load "life-groove.png")))
    (set! %%life-groove (image-opengl-upload
                         (CGImageRef-data image)
                         (CGImageRef-width image)
                         (CGImageRef-height image))))

  (let ((image (CGImageRef-load "life-bar.png")))
    (set! %%life-bar (image-opengl-upload
                      (CGImageRef-data image)
                      (CGImageRef-width image)
                      (CGImageRef-height image)))))

(define (render-life)
  (let ((health (exact->inexact (/ %%life %%max-life))))
    (glLoadIdentity)
    (glColor4f 1. 1. 1. 1.)

    (glLoadIdentity)
    (glTranslatef 0. .95 0.)
    (glScalef 1. .05 1.)
    (image-render-base)
    
    (glEnable GL_BLEND)
    (glBlendFunc GL_ONE GL_ONE_MINUS_SRC_ALPHA)
    
    (glLoadIdentity)
    (glTranslatef (- health 1.) .95 0.)
    (glScalef 1. .05 1.)
    (glColor4f 1. 1. 1. 1.)
    (image-render %%life-bar)

    (glLoadIdentity)
    (glTranslatef 0. .95 0.)
    (glScalef 1. .05 1.)
    (image-render %%life-groove)
    
    (glDisable GL_BLEND)))

(define (get-health)
  (/ %%life %%max-life))

(define %%shatter-source #f)
(define (on-death)
  (if (not %%shatter-source)
      (let ((source (make-audio-source shatter-audio)))
        (play-audio source)
        (set! %%shatter-source source))))

;; ================= levels ================

;;;; score

(define LVL_SCORE 0)
(define LVL_RAW_SCORE 0)

(define (update-score obj)
  (let* ((mesh (scene-object-mesh obj))
         (points
          (cond
           ((eq? mesh chicken-mesh) .5)
           ((eq? mesh duck-mesh) 1)
           ((eq? mesh sheep-mesh) 1)
           ((eq? mesh cow-mesh) 2))))
    (set! LVL_SCORE (+ LVL_SCORE points))
    (set! LVL_RAW_SCORE (+ LVL_RAW_SCORE 1))))

(define (get-score)
  LVL_SCORE)

(define (get-raw-score)
  LVL_RAW_SCORE)

(define (reset-score)
  (set! LVL_SCORE 0)
  (set! LVL_RAW_SCORE 0))

;;;; defaults

(define default-frequency 2.5)

(define (default-get-mesh)
  (let ((x (random-integer 4)))
    (case x
      ((0) cow-mesh)
      ((1) sheep-mesh)
      ((2) duck-mesh)
      ((3) chicken-mesh))))

(define (default-atmosphere)
  (glLoadIdentity)

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
  (set! GRAVITY (make-vec3d 0. -11. 0.)))

(define (default-level-config)
  (set! LVL_FREQUENCY default-frequency)
  (set! LVL_GET_MESH default-get-mesh)
  (set! LVL_ATMOSPHERE default-atmosphere))

(define LVL_FREQUENCY #f)
(define LVL_GET_MESH #f)
(define LVL_ATMOSPHERE #f)

(define-type level
  name
  begin
  run
  end
  next)

(include "levels.scm")

(define current-level (make-parameter #f))

(define (switch-to-level! level)
  (if (current-level)
      ((level-end (current-level))))
  (current-level level)
  (default-level-config)
  ((level-begin (current-level)))
  (LVL_ATMOSPHERE)
  (reset-score))

(define (next-level!)
  (switch-to-level! ((level-next (current-level)))))

;; engine
(define background-texture #f)
(define sky-texture #f)
(define gradient-texture #f)
(define line-texture #f)
(define line2-texture #f)
(define star-texture #f)
(define bah-audio #f)
(define moo-audio #f)
(define chicken-audio #f)
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

  (let ((image (CGImageRef-load "gradient.png")))
    (set! gradient-texture (image-opengl-upload
                            (CGImageRef-data image)
                            (CGImageRef-width image)
                            (CGImageRef-height image))))

  (let ((image (CGImageRef-load "sky.png")))
    (set! sky-texture (image-opengl-upload
                            (CGImageRef-data image)
                            (CGImageRef-width image)
                            (CGImageRef-height image))))
  
  (let ((line (CGImageRef-load "line.png"))
        (line2 (CGImageRef-load "line2.png"))
        (star (CGImageRef-load "star.png")))
    (set! line-texture (image-opengl-upload
                        (CGImageRef-data line)
                        (CGImageRef-width line)
                        (CGImageRef-height line)))
    (set! line2-texture (image-opengl-upload
                         (CGImageRef-data line2)
                         (CGImageRef-width line2)
                         (CGImageRef-height line2)))
    (set! star-texture (image-opengl-upload
                        (CGImageRef-data star)
                        (CGImageRef-width star)
                        (CGImageRef-height star))))
  
  (init-audio)
  (set! bah-audio (load-audio "bah.wav"))
  (set! moo-audio (load-audio "moo.wav"))
  (set! thud-audio (load-audio "thud.wav"))
  (set! shatter-audio (load-audio "shatter.wav"))
  (set! explosion-audio (load-audio "explosion2.wav"))
  (set! chicken-audio (load-audio "chicken.wav"))

  (life-init)
  
  (switch-to-level! (make-level1)))

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
    (if background-texture
        (begin
         (glMatrixMode GL_PROJECTION)
         (glLoadIdentity)
         (glOrthof 0.0 1.0 1.0 0.0 -1.0 1.0)
         (glMatrixMode GL_MODELVIEW)
         (glLoadIdentity)
         (glColor4f 1. 1. 1. 1.)
         (image-render background-texture)))

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
    ((level-run (current-level)))

    ;; Post 2d
    (glMatrixMode GL_PROJECTION)
    (glLoadIdentity)
    (ortho 0.0 1.0 1.0 0.0 -1.0 1.0)
    (glMatrixMode GL_MODELVIEW)

    (if (not (no-more-life?))
        (render-cracks)
        (on-death))

    (render-life)

    (glMatrixMode GL_PROJECTION)
    (glLoadIdentity)
    (ortho 0.0 1.0 1.5 0.0 -1.0 1.0)
    (glMatrixMode GL_MODELVIEW)
    (render-lasers))
  (##gc))

(define (get-title)
  "")
