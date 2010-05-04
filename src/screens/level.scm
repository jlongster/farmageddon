;;; level screen
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
(include "../events.scm")
(include "../fog.scm")
(include "../game-events.scm")

;; resources

(define steak-mesh (obj-load (resource "steak") #t))
(define bones-mesh (obj-load (resource "bones") #t))
(define chicken-mesh (obj-load (resource "chicken") #t))
(define duck-mesh (obj-load (resource "duck") #t))
(define cow-mesh (obj-load (resource "cow") #t))
(define cow-part1-mesh (obj-load (resource "cow-part1") #t))
(define cow-part2-mesh (obj-load (resource "cow-part2") #t))
(define cow-part3-mesh (obj-load (resource "cow-part3") #t))
(define person-mesh (obj-load (resource "person") #t))
(define sheep-mesh (obj-load (resource "sheep") #t))
(define pig-mesh (obj-load (resource "pig") #t))
(define pig-part1-mesh (obj-load (resource "pig-part1") #t))
(define pig-part2-mesh (obj-load (resource "pig-part2") #t))

(define level-bg #f)
(define fog-texture #f)
(define star-texture #f)

(define sheep1-audio #f)
(define sheep2-audio #f)
(define cow1-audio #f)
(define cow2-audio #f)
(define chicken1-audio #f)
(define chicken2-audio #f)
(define pig1-audio #f)
(define duck1-audio #f)
(define scream-audio)
(define thud-audio #f)
(define shatter-audio #f)
(define lightning-audio #f)
(define explosion1-audio #f)
(define explosion2-audio #f)
(define explosion3-audio #f)
(define explosion4-audio #f)
(define fog-audio #f)
(define clap-audio #f)

(define default-font50 #f)
(define default-font24 #f)

;; util

(define (random-in-range x #!optional y)
  (let* ((high (if y y x))
        (low (if y x (- x)))
        (diff (- high low)))
    (+ (* (random-real) diff) low)))

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

(define (darken f)
  (overlay-list-add
   (make-tween
    (make-2d-object
     2d-perspective
     color: (make-vec4d 0. 0. 0. 0.))
    alpha: .6
    length: 1.
    type: 'ease-out-cubic
    on-finished: (let ((done #f))
                   (lambda ()
                     (if (not done)
                         (f))
                     (set! done #t))))
   important: #t))

(define (add-centered-font font msg y #!optional size)
  (let* ((width (UIView-width (current-view)))
         (advance (ftgl-get-font-advance font
                                         msg))
         (font-width (if size
                         (* advance (/ size (ftgl-get-font-face-size
                                               font)))
                         advance))
         (obj (make-2d-object
               font-perspective
               font: (make-2d-font font msg size)
               color: (make-vec4d 1. 1. 1. 1.)
               position: (make-vec3d (/ (exact->inexact width) 2.)
                                     y
                                     0.)
               center: (make-vec3d (/ font-width 2.)
                                   0.
                                   0.))))
    (overlay-list-add obj important: #t)
    obj))

(define (add-tweened obj)
  (generic-object-color-set! obj (make-vec4d 1. 1. 1. 0.))
  (overlay-list-add
   (make-tween
    obj
    alpha: 1.
    length: .5)
   important: #t))

(define (add-centered-mesh mesh #!optional scale)
  (let ((scale (or scale 1.)))
    (overlay-list-add
     (make-mesh-object
      3d-perspective
      mesh: mesh
      position: (make-vec3d 0. .85 5.)
      rotation: (make-vec4d 1. 1. 0. 0.)
      scale: (make-vec3d scale scale scale)
      update: (lambda (this)
                (let ((rot (mesh-object-rotation this)))
                  (mesh-object-rotation-set!
                   this
                   (make-vec4d (+ (vec4d-x rot) .1)
                               (+ (vec4d-y rot) .1)
                               (+ (vec4d-z rot) .1)
                               (+ (vec4d-w rot) 1.))))))
     important: #t)))

(define (on-trial-ended)
  (darken
   (lambda ()
     ;;(add-centered-font default-font24 "~ LITE VERSION ENDED ~" 380. 18.)
     (add-centered-font default-font24 "THANKS FOR PLAYING!" 360. 24.)
     (add-centered-font default-font24 "BUY THE FULL VERSION" 335. 18.)
     (add-centered-font default-font24 "TO SCORE HIGHER!" 315. 18.)
     (on-complete))))

(define (on-fail)
  (darken
   (lambda ()
     (add-centered-font default-font24 "~ YOU KILLED A HUMAN ~" 380. 18.)
     (add-centered-mesh person-mesh)
     (on-complete))))

(define (on-death)
  (let ((source (make-audio-source shatter-audio)))
    (shatter)

    ;; (for-each (lambda (obj)
    ;;             (if (and (2d-object? obj)
    ;;                      (eq? (2d-object-texture obj)
    ;;                           glare-texture))
    ;;                 (overlay-list-remove obj)))
    ;;           overlay-list)

    (play-audio source)

    (thread-start!
     (make-thread
      (lambda ()
        (thread-sleep! 1.)
        (free-audio-source source)))))

  (let* ((victor (mesh-object-mesh (player-animal-victor)))
         (line (string-append "~ "
                             (cond
                              ((eq? victor chicken-mesh) "A CHICKEN")
                              ((eq? victor duck-mesh) "A DUCK")
                              ((eq? victor sheep-mesh) "A SHEEP")
                              ((eq? victor pig-mesh) "A PIG")
                              ((eq? victor pig-part1-mesh) "A PIG HEAD")
                              ((eq? victor pig-part2-mesh) "A PIG BUTT")
                              ((eq? victor cow-mesh) "A COW")
                              ((eq? victor cow-part1-mesh) "A COW HEAD")
                              ((eq? victor cow-part2-mesh) "COW UDDERS")
                              ((eq? victor cow-part3-mesh) "A COW BUTT")
                              (else ""))
                             " KILLED YOU ~")))
    (darken
     (lambda ()
       (add-centered-font default-font24 line 380. 18.)
       (add-centered-mesh victor (if (or (eq? victor cow-mesh)
                                         (eq? victor cow-part1-mesh)
                                         (eq? victor cow-part2-mesh)
                                         (eq? victor cow-part3-mesh)) .7 1.))
       (on-complete)))))

(define (on-win)
  (play-voice clap-audio)
  (let loop ((ang 0.))
    (if (<= ang 360.)
        (begin
          (overlay-list-add
           (make-tween
            (make-2d-object
             2d-ratio-perspective
             texture: star-texture
             scale: (make-vec3d .1 .1 1.)
             position: (make-vec3d .58 .84 0.)
             color: (make-vec4d 1. 1. 1. 1.)
             rotation: (make-vec4d 0. 0. 1. 0.)
             center: #t)
            position: (make-vec3d (+ .58 (* (sin ang) .5))
                                  (+ .84 (* (cos ang) .5))
                                  0.)
            rotation: (make-vec4d 0. 0. 1. 360.)
            alpha: 0.
            length: .5)
           important: #t)
          (loop (+ ang 36.))))))

(define (on-complete)
  (stop-event-executioner)
  (score-remove)

  (add-centered-font default-font50 "GAME OVER" 410. 30.)

  (let ((top .07))
    (add-tweened
     (make-2d-object
      font-perspective
      font: (make-2d-font default-font24
                          (string-append "NAME:")
                          20.)
      position: (to-font-space .295 (+ .435 top))))

    (let ((width (UIView-width (current-view)))
          (height (UIView-height (current-view))))
      (show-high-score-field (inexact->exact (floor (* .52 width)))
                             (inexact->exact (floor (+ (* .4575 height) top)))))

    (add-tweened
     (make-2d-object
      font-perspective
      font: (make-2d-font default-font24
                          "SCORE:"
                          20.)
      position: (to-font-space .27 (+ .51 top))))

    (add-tweened
     (make-2d-object
      font-perspective
      font: (make-2d-font default-font24
                          (number->string (score))
                          20.)
      position: (to-font-space .52 (+ .51 top))))

    (let* ((scores (get-high-scores))
           (high-score (if (null? scores)
                           0
                           (persistent-score-score (car scores)))))
      (add-tweened
       (make-2d-object
        font-perspective
        font: (make-2d-font default-font24
                            (string-append "HIGH SCORE:")
                            20.)
        position: (to-font-space .112 (+ .57 top))))

      (add-tweened
       (make-2d-object
        font-perspective
        font: (make-2d-font default-font24
                            (number->string high-score)
                            20.)
        position: (to-font-space .52 (+ .57 top))))

      (if (> (score) high-score)
          (on-win)))

    (overlay-add-button "TRY AGAIN"
                        (make-vec2d .25 (+ .63 top))
                        .5 1.
                        (lambda (this)
                          (fog-list-clear!)
                          (hide-high-score-field)
                          (set-screen! level-screen)))
    (overlay-add-button "MENU"
                        (make-vec2d .25 (+ .75 top))
                        .5 1.
                        (lambda (this)
                          (fog-list-clear!)
                          (hide-high-score-field)
                          (set-screen! title-screen)))
    
    (save-score)))

;; init

(define font-perspective #f)

(define (level-screen-init)
  (let* ((width (UIView-width (current-view)))
         (height (UIView-height (current-view)))
         (3d-pers (perspective 40.
                               (exact->inexact (/ width height))
                               1. 1000.))
         (initial-matrix (perspective-matrix 3d-pers))
         (lookat-matrix (lookat (make-vec3d 0. 0. 0.)
                                (make-vec3d 0. 0. 1.)
                                (make-vec3d 0. 1. 0.))))
    
    (perspective-matrix-set!
     3d-pers
     (4x4matrix-multiply lookat-matrix initial-matrix))

    (free initial-matrix)
    (free lookat-matrix)
    
    (set! 3d-perspective 3d-pers)

    (set! font-perspective
          (ortho 0 (exact->inexact width)
                 0 (exact->inexact height)
                 -10000.0 10000.0)))

  (set! level-bg (image-opengl-load "background.pvr"))
  (set! fog-texture (image-opengl-load "fog.pvr"))
  (set! star-texture (image-opengl-load "star.pvr"))

  (glBindTexture GL_TEXTURE_2D (img-id fog-texture))
  (glTexParameteri GL_TEXTURE_2D
                   GL_TEXTURE_MIN_FILTER
                   GL_LINEAR)
  (glTexParameteri GL_TEXTURE_2D
                   GL_TEXTURE_MAG_FILTER
                   GL_LINEAR)
  (glBindTexture GL_TEXTURE_2D 0)
  
  (weapons-init)
  
  (init-audio)
  (set! sheep1-audio (load-audio "sheep1.wav"))
  (set! sheep2-audio (load-audio "sheep2.wav"))
  (set! cow1-audio (load-audio "cow1.wav"))
  (set! cow2-audio (load-audio "cow2.wav"))
  (set! pig1-audio (load-audio "pig1.wav"))
  (set! chicken1-audio (load-audio "chicken1.wav"))
  (set! chicken2-audio (load-audio "chicken2.wav"))
  (set! duck1-audio (load-audio "duck1.wav"))
  (set! scream-audio (load-audio "scream.wav"))
  (set! thud-audio (load-audio "thud.wav"))
  (set! shatter-audio (load-audio "shatter.wav"))
  (set! lightning-audio (load-audio "lightning.wav"))
  (set! explosion1-audio (load-audio "explosion1.wav"))
  (set! explosion2-audio (load-audio "explosion2.wav"))
  (set! explosion3-audio (load-audio "explosion3.wav"))
  (set! explosion4-audio (load-audio "explosion4.wav"))
  (set! fog-audio (load-audio "fog.wav"))
  (set! clap-audio (load-audio "clap.wav"))
  
  (set! default-font50
        (ftgl-create-texture-font (resource "ApexSansExtraBoldC.ttf")))
  (set! default-font24
        (ftgl-create-texture-font (resource "ApexSansExtraBoldC.ttf")))
  
  (ftgl-set-font-face-size default-font50 50)
  (ftgl-get-font-advance
   default-font50
   "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789~-:!>")

  (ftgl-set-font-face-size default-font24 24)
  (ftgl-get-font-advance
   default-font24
   "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789~-:/!.>")
    
  (load-randomized-cracks)
  
  (overlay-init)
  (scene-init)
  (fog-init))

;; setup the scene

(define *background-object* #f)

(define (level-screen-setup)
  (glEnable GL_DEPTH_TEST)
  (glEnable GL_CULL_FACE)
  (glCullFace GL_BACK)
  (glShadeModel GL_SMOOTH)
  (glEnable GL_RESCALE_NORMAL)

  (scene-list-clear!)
  
  (overlay-list-add
   (make-scene-object
    2d-ratio-perspective
    (lambda (self)
      (life-render))
    values))

  (reset-player)
  (stop-event-executioner)
  (set-difficulty! 0)

  (set! *background-object*
        (make-2d-object
         2d-perspective
         texture: (current-background-texture)))

  (scene-list-add *background-object* unimportant: #t)
  (fog-list-clear!)

  (add-counter))

(define (background-pop color)
  (scene-list-remove *background-object*)
  (set! *background-object*
        (make-tween
         (make-2d-object
          2d-perspective
          texture: (current-background-texture)
          color: color)
         color: (make-vec4d 1. 1. 1. 1.)
         type: 'ease-out-quad))
  (scene-list-add *background-object* unimportant: #t))

;; updating and processing events

(define (level-screen-run)
  (load-perspective 3d-perspective)
  (handle-intersections)

  (scene-list-update global-update)
  (overlay-update)
  (player-update)
  (update-weapons)
  (update-fog)
  (check-difficulty)

  (update-counter (current-difficulty))
  
  (if (not (player-finished?))
      (run-events)))

;; rendering

(define (level-screen-render)
  (load-perspective 3d-perspective)
  
  ;; 3d
  (scene-list-render)

  ;; overlay
  (load-perspective 2d-ratio-perspective)
  (render-weapons)

  (render-fog)

  (load-perspective 2d-ratio-perspective)
  (if (not (player-finished?))
      (begin
        (render-cracks)))

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
