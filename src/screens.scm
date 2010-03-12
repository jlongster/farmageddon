;;;; screens
;;; This implements a system that handles transitions between various
;;; menus, screens, and levels in the game.

(declare (block)
         (standard-bindings)
         (extended-bindings))

(define CURRENT-SCREEN #f)
(define SCREEN-LIST '())

(define-type screen
  id: D0B1BFA4-123E-443D-80A3-3F88BA51D4F1
  name
  init
  setup
  run
  render
  touches-began)

(define-macro (define-screen name #!key
          init
          setup
          run
          render
          touches-began)
  `(begin
     (define ,name (make-screen ,(symbol->string name)
                                ,init
                                ,setup
                                (lambda () (,run))
                                (lambda () (,render))
                                ,touches-began))
     (set! SCREEN-LIST (cons ,name SCREEN-LIST))))

(define NEED-INIT #t)

(define (run-initializers)
  (for-each (lambda (screen)
              ((screen-init screen)))
            SCREEN-LIST))

(define (set-screen! screen)
  (if NEED-INIT
      (begin
        (run-initializers)
        (set! NEED-INIT #f)))

  (scene-list-clear!)
  (overlay-list-clear!)
  ((screen-setup screen))
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

(define 2d-perspective
  (ortho 0.0 1.0 1.0 0.0 -1.0 1.0))

(define 2d-ratio-perspective
  (ortho 0.0 1.0 1.5 0.0 -1.0 1.0))

(include "screens/title.scm")
(include "screens/level.scm")
(include "screens/level-name.scm")
