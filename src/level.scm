;;;; level screen
;;; Implements the screen which actually plays the game

;; libraries

(include "weapons.scm")
(include "glass.scm")
(include "player.scm")
(include "overlay.scm")
(include "picking.scm")
(include "levels.scm")
(include "entity.scm")

;; resources

(define chicken-mesh (obj-load (resource "chicken") #t))
(define duck-mesh (obj-load (resource "duck") #t))
(define cow-mesh (obj-load (resource "cow") #t))
(define cow-part1-mesh (obj-load (resource "cow-part1") #t))
(define cow-part2-mesh (obj-load (resource "cow-part2") #t))
(define cow-part3-mesh (obj-load (resource "cow-part3") #t))
(define person-mesh (obj-load (resource "person") #t))
(define sheep-mesh (obj-load (resource "sheep") #t))

(define level1-texture #f)
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

(define (on-win)
  (overlay-list-add
   (make-tween
    (make-2d-object
     2d-perspective
     texture: success-texture
     color: (make-vec4d 1. 1. 1. 0.))
    alpha: 1.
    length: .5)))

(define (on-fail)
  (overlay-list-add
   (make-tween
    (make-2d-object
     2d-perspective
     texture: fail-texture
     color: (make-vec4d 1. 1. 1. 0.))
    alpha: 1.
    length: .5)))

(define SHATTER-SOUND #f)
(define (on-death)
  (if (not SHATTER-SOUND)
      (let ((source (make-audio-source shatter-audio)))
        (play-audio source)
        (set! SHATTER-SOUND source))))


;; init

(define apex-font #f)
(define batang-font #f)
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

  (glEnable GL_DEPTH_TEST)
  (glEnable GL_CULL_FACE)
  (glCullFace GL_BACK)
  (glShadeModel GL_SMOOTH)
  (glEnable GL_RESCALE_NORMAL)

  (set! gradient-texture (image-opengl-load "gradient.png"))
  (set! level1-texture (image-opengl-load "level-screen1.png"))
  (set! fail-texture (image-opengl-load "fail.png"))
  (set! success-texture (image-opengl-load "success.png"))
  
  (weapons-init)
    
  (init-audio)
  (set! bah-audio (load-audio "bah.wav"))
  (set! moo-audio (load-audio "moo.wav"))
  (set! thud-audio (load-audio "thud.wav"))
  (set! shatter-audio (load-audio "shatter.wav"))
  (set! explosion-audio (load-audio "explosion2.wav"))
  (set! chicken-audio (load-audio "chicken.wav"))

  (set! apex-font (ftgl-create-texture-font (resource "ApexSansBookC.ttf")))
  (set! batang-font (ftgl-create-texture-font (resource "Batang.ttf")))
  
  (load-randomized-cracks)
  
  (set-level! (basic-level))
  (overlay-init))

;; updating and processing events

(define (level-screen-run)
  (load-perspective 3d-perspective)

  (handle-intersections)
  (scene-list-update global-update)
  (scene-list-update values)

  (overlay-update)
  (update-weapons)

  (if (not (or (life-is-dead?)
               (goal-met?)
               (goal-failed?)))
      (possibly-make-entity)))

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

  (render-weapons)
  (if (not (life-is-dead?))
      (render-cracks))
  (overlay-render))

(define (level-screen-touches-began touches event)
  (for-each (lambda (el)
              (let ((loc (UITouch-location el)))
                (queue-intersection (car loc) (cdr loc))
                (add-hit-point loc)))
            touches))

(define-screen level-screen
  init: level-screen-init
  run: level-screen-run
  render: level-screen-render
  touches-began: level-screen-touches-began)
