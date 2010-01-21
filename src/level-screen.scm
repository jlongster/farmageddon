;;;; level screen
;;; Implements the screen which actually plays the game

;; libraries

(include "weapons.scm")
(include "glass.scm")
(include "player.scm")
(include "overlay.scm")
(include "picking.scm")
(include "levels.scm")

;; resources

(define chicken-mesh (obj-load (resource "chicken") #t))
(define duck-mesh (obj-load (resource "duck") #t))
(define cow-mesh (obj-load (resource "cow") #t))
(define sheep-mesh (obj-load (resource "sheep") #t))

(define sky-texture #f)
(define gradient-texture #f)
(define bah-audio #f)
(define moo-audio #f)
(define chicken-audio #f)
(define thud-audio #f)
(define shatter-audio #f)
(define explosion-audio #f)

;; util

(define (spread-number fl)
  (- (* fl 2.) 1.))

(define 3d-projection-matrix #f)

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

;; init

(define (level-screen-init)

  (set! 3d-projection-matrix
        (4x4matrix-multiply
         (perspective 40.
                      (/ (UIView-width (current-view))
                         (UIView-height (current-view)))
                      1. 1000.)
         (lookat (make-vec3d 0. 0. 0.)
                 (make-vec3d 0. 0. 1.)
                 (make-vec3d 0. 1. 0.))))

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

  (weapons-init)
    
  (init-audio)
  (set! bah-audio (load-audio "bah.wav"))
  (set! moo-audio (load-audio "moo.wav"))
  (set! thud-audio (load-audio "thud.wav"))
  (set! shatter-audio (load-audio "shatter.wav"))
  (set! explosion-audio (load-audio "explosion2.wav"))
  (set! chicken-audio (load-audio "chicken.wav"))

  (overlay-init)
  (load-randomized-cracks)

  (set-level! (basic-level)))

;; updating and processing events

(define (level-screen-run)
  (setup-3d-scene)

  (handle-intersections)
  (scene-list-update global-update)
  
  (if (goal-met?)
      (on-win)
      (possibly-make-entity))
  
  (if (life-is-dead?)
      (on-death)))


(define (on-win)
  =)

(define SHATTER-SOUND #f)
(define (on-death)
  (if (not SHATTER-SOUND)
      (let ((source (make-audio-source shatter-audio)))
        (play-audio source)
        (set! SHATTER-SOUND source))))

;; rendering

(define (level-screen-render)
  (glClearColor 0. 0. 0. 1.)
  (glClear (bitwise-ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))

  ;; background
  (if (current-background-texture)
      (begin
        (glMatrixMode GL_PROJECTION)
        (glLoadIdentity)
        (glOrthof 0.0 1.0 1.0 0.0 -1.0 1.0)
        (glMatrixMode GL_MODELVIEW)
        (glLoadIdentity)
        (glColor4f 1. 1. 1. 1.)
        (image-render (current-background-texture))))

  ;; 3d
  (setup-3d-scene)
  (scene-list-render)

  ;; overlay

  (ortho 0.0 1.0 1.5 0.0 -1.0 1.0)


  (if (not (life-is-dead?))
      (render-cracks))
  
  (overlay-render)
  (render-weapons))

(define (level-screen-touches-began touches event)
  (for-each (lambda (el)
              (let ((loc (UITouch-location el)))
                (queue-intersection (car loc) (cdr loc))
                (add-hit-point loc)
                ))
            touches))

(define-screen level-screen
  init: level-screen-init
  run: level-screen-run
  render: level-screen-render
  touches-began: level-screen-touches-began)
