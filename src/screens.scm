;;;; screens
;;; This implements a system that handles transitions between various
;;; menus, screens, and levels in the game.

(define CURRENT-SCREEN #f)

(define-type screen
  id: D0B1BFA4-123E-443D-80A3-3F88BA51D4F1
  name
  has-init
  init
  run
  render
  touches-began)

(define-macro (define-screen name #!key
          init
          run
          render
          touches-began)
  `(define ,name (make-screen ,(symbol->string name)
                              #f
                              ,init
                              (lambda () (,run))
                              (lambda () (,render))
                              ,touches-began)))

(define (set-screen! screen)
  (scene-list-clear!)
  (set! CURRENT-SCREEN screen))

(define (current-screen-run)
  ((screen-run CURRENT-SCREEN)))

(define (current-screen-render)
  ((screen-render CURRENT-SCREEN)))

(define (current-screen)
  CURRENT-SCREEN)

;; touch events

(define-event-handler (touches-began touches event)
  (current-screen-touches-began touches event))

(define (current-screen-touches-began touches event)
  (and-let* ((touches-began
              (screen-touches-began CURRENT-SCREEN)))
    (touches-began touches event)))

;; title screen

(define title-texture #f)

(define 2d-perspective
  (ortho 0.0 1.0 1.0 0.0 -1.0 1.0))

(define 2d-ratio-perspective
  (ortho 0.0 1.0 1.5 0.0 -1.0 1.0))

(define-screen title-screen
  init: (lambda ()
          (set! title-texture
                (image-opengl-load "title-screen.png"))
          (scene-list-add
           (make-2d-object
            2d-perspective
            update: (lambda (obj)
                      (eq? (current-screen) title-screen))
            texture: title-texture)))
  run: (lambda ()
         (scene-list-update))
  render: (lambda ()
            (scene-list-render))
  touches-began: (lambda (touches event)
                   (scene-list-add
                    (make-tween
                     (make-2d-object
                      2d-perspective
                      color: (make-vec4d 0. 0. 0. 0.))
                     length: .5
                     alpha: 1.
                     on-finished:
                     (lambda ()
                       (set-screen! level-screen)
                       (scene-list-add
                        (make-tween
                         (make-2d-object
                          2d-perspective
                          color: (make-vec4d 0. 0. 0. 1.))
                         length: 2.5
                         alpha: 0.
                         on-finished: (lambda () #f)))
                       #f)))))

(include "tests/screen.scm")
(include "level.scm")
