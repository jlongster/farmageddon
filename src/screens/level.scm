;;;; level screen
;;; Implements the screen which actually plays the game

(declare (block)
         (standard-bindings)
         (extended-bindings))

;; libraries

(include "../weapons.scm")
(include "../glass.scm")
(include "../player.scm")
(include "../overlay.scm")
(include "../picking.scm")
(include "../levels.scm")
(include "../entity.scm")

;; resources

(define chicken-mesh (obj-load (resource "chicken") #t))
(define duck-mesh (obj-load (resource "duck") #t))
(define cow-mesh (obj-load (resource "cow") #t))
(define cow-part1-mesh (obj-load (resource "cow-part1") #t))
(define cow-part2-mesh (obj-load (resource "cow-part2") #t))
(define cow-part3-mesh (obj-load (resource "cow-part3") #t))
(define person-mesh (obj-load (resource "person") #t))
(define sheep-mesh (obj-load (resource "sheep") #t))

(define level-bg1 #f)
(define level-bg2 #f)
(define level-bg3 #f)
(define glare-texture #f)
(define gradient-texture #f)
(define fail-texture #f)
(define success-texture #f)

(define bah-audio #f)
(define moo-audio #f)
(define chicken-audio #f)
(define thud-audio #f)
(define shatter-audio #f)
(define explosion-audio #f)
 
;; util

(define (spread-number fl)
  (- (* fl 2.) 1.))

(define 3d-perspective #f)

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

;; events

(define (darken)
  (overlay-list-add
   (make-tween
    (make-2d-object
     2d-perspective
     color: (make-vec4d 0. 0. 0. 0.))
    alpha: .6
    length: .1)
   important: #t))

(define (add-centered-font font msg y #!optional size)
  (let* ((width (UIView-width (current-view)))
         (advance (ftgl-get-font-advance font
                                         msg))
         (font-width (if size
                         (* advance (/ size (ftgl-get-font-face-size
                                               font)))
                         advance)))
    (overlay-list-add
     (make-tween
      (make-2d-object
       font-perspective
       font: (make-2d-font font msg size)
       color: (make-vec4d 1. 1. 1. 0.)
       position: (make-vec3d (/ (exact->inexact width) 2.)
                             y
                             0.)
       center: (make-vec3d (/ font-width 2.)
                           0.
                           0.))
      alpha: 1.
      length: .5)
     important: #t)))

(define (on-win)
  (darken)
  (add-centered-font big-name-font "SUCCESS!" 300.)
  (add-centered-font apex-font "live long, soldier" 240. 24.))

(define (on-fail)
  (darken)
  (add-centered-font big-name-font "YOU" 330.)
  (add-centered-font big-name-font "FAILED!" 275.)

  (add-centered-font apex-font
                     "the farmageddon" 220. 24.)
  (add-centered-font apex-font
                     "has destroyed the world" 190. 24.))

(define (on-death)
  (let ((source (make-audio-source shatter-audio)))
    (shatter)

    (for-each (lambda (obj)
                (if (and (2d-object? obj)
                         (eq? (2d-object-texture obj)
                              glare-texture))
                    (overlay-list-remove obj)))
              overlay-list)
    
    (play-audio source)

    (thread-start!
     (make-thread
      (lambda ()
        (thread-sleep! 1.)
        (free-audio-source source))))))

(define (on-complete)
  (let ((time (real-time)))
    (overlay-list-add
     (make-scene-object
      3d-perspective
      values
      (lambda (this)
        (if (> (- (real-time) time)
               2.)
            (begin
              (overlay-list-remove this)
              (overlay-list-add
               (make-tween
                (make-2d-object
                 2d-perspective
                 color: (make-vec4d 0. 0. 0. 0.))
                alpha: 1.
                length: 1.
                type: 'ease-in-cubic
                on-finished: (lambda ()
                               (let ((failed (player-failed?)))
                                 (reset-player)
                                 (if (not failed)
                                     (if (not (next-level))
                                         (on-game-won)
                                         (set-screen! level-name-screen))
                                     (set-screen! level-name-screen)))))
               important: #t))))))))

(define (on-game-won)
  (overlay-list-clear!)
  (darken)
  (add-centered-font big-name-font "GAME OVER" 330.))

;; init

(define apex-font #f)
(define font-perspective #f)

(define (level-screen-init)
  (let* ((width (UIView-width (current-view)))
         (height (UIView-height (current-view)))
         (3d-pers (perspective 40.
                               (exact->inexact (/ width height))
                               1. 1000.)))
    (perspective-matrix-set!
     3d-pers
     (4x4matrix-multiply
      (lookat (make-vec3d 0. 0. 0.)
              (make-vec3d 0. 0. 1.)
              (make-vec3d 0. 1. 0.))
      (perspective-matrix 3d-pers)))
    (set! 3d-perspective 3d-pers)

    (set! font-perspective
          (ortho 0 (exact->inexact width)
                 0 (exact->inexact height)
                 -10000.0 10000.0)))

  (set! gradient-texture (image-opengl-load "gradient.png"))
  (set! level-bg1 (image-opengl-load "level-bg1.png"))
  (set! level-bg2 (image-opengl-load "level-bg2.png"))
  (set! level-bg3 (image-opengl-load "level-bg3.png"))
  (set! glare-texture (image-opengl-load "level-glare.png"))
 
  (weapons-init)
  
  (init-audio)
  (set! bah-audio (load-audio "bah.wav"))
  (set! moo-audio (load-audio "moo.wav"))
  (set! thud-audio (load-audio "thud.wav"))
  (set! shatter-audio (load-audio "shatter.wav"))
  (set! explosion-audio (load-audio "explosion2.wav"))
  (set! chicken-audio (load-audio "chicken.wav"))

  (set! apex-font (ftgl-create-texture-font (resource "ApexSansBookC.ttf")))
  (ftgl-set-font-face-size apex-font 72)
  (ftgl-get-font-advance apex-font "1234567890:-")

  (load-randomized-cracks)
  
  (overlay-init)
  (first-level))

;; setup the scene

(define (level-screen-setup)
  (glEnable GL_DEPTH_TEST)
  (glEnable GL_CULL_FACE)
  (glCullFace GL_BACK)
  (glShadeModel GL_SMOOTH)
  (glEnable GL_RESCALE_NORMAL)

  (overlay-setup)

  (overlay-list-add
   (make-2d-object
    2d-perspective
    texture: glare-texture))

  (overlay-list-add
   (make-scene-object
    2d-ratio-perspective
    (lambda (self)
      (life-render))
    values))
  
  (player-init))

;; updating and processing events

(define (level-screen-run)
  (load-perspective 3d-perspective)

  (handle-intersections)

  (scene-list-update global-update)
  (overlay-update)
  (player-update)
  (update-weapons)
  
  (if (current-level)
      (if (not (player-finished?))
          (possibly-make-entity))))

;; rendering

(define (level-screen-render)
  ;; background
  (if (current-background-texture)
      (begin
        (load-perspective 2d-perspective)
        (glColor4f 1. 1. 1. 1.)
        (image-render (current-background-texture))))

  ;; 3d
  (scene-list-render)

  ;; overlay
  (load-perspective 2d-ratio-perspective)

  (if (current-level)
      (begin
        (render-weapons)
        (if (not (player-finished?))
            (render-cracks))))

  (load-perspective 2d-ratio-perspective)
  (overlay-render))

(define (level-screen-touches-began touches event)
  (if (not (player-finished?))
      (for-each (lambda (el)
                  (let ((loc (UITouch-location el)))
                    (queue-intersection (car loc) (cdr loc))
                    (add-hit-point loc)))
                touches)))

(define-screen level-screen
  init: level-screen-init
  setup: level-screen-setup
  run: level-screen-run
  render: level-screen-render
  touches-began: level-screen-touches-began)
