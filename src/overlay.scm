;;;; overlay 
;;; Controls and renders all of the 2d components

(declare (block)
         (standard-bindings)
         (extended-bindings))

;; util

(define (to-font-space x y)
  (let ((width (UIView-width (current-view)))
        (height (UIView-height (current-view))))
    (make-vec3d (* x width)
                (* (- 1. y) height)
                0.)))

;; custom scene list

(define overlay-list '())

(define (overlay-list-clear!)
  (set! overlay-list '())
  (buttons-clear!))

(define (overlay-list-add obj #!key important)
  (if important
      (set! overlay-list (append overlay-list (list obj)))
      (set! overlay-list (cons obj overlay-list))))

(define overlay-list-remove scene-list-remove)

(define (overlay-render)
  (scene-list-render #f overlay-list))

(define (overlay-update)
  (scene-list-purely-updates #f #f overlay-list)
  (set! overlay-list
        (scene-list-purely-removes overlay-list)))

;; life

(define TX-LIFE-GROOVE #f)
(define TX-LIFE-BAR #f)
(define TX-BUTTON #f)

(define (life-render)
  (glColor4f 1. 1. 1. 1.)

  (glLoadIdentity)
  (glTranslatef 0. 1.425 0.)
  (glScalef 1. .075 1.)
  (image-render-base)

  (glEnable GL_BLEND)
  (glBlendFunc GL_ONE GL_ONE_MINUS_SRC_ALPHA)

  (glLoadIdentity)
  (glTranslatef (- (exact->inexact (life-ratio)) 1.)
                1.425
                0.)
  (glScalef 1. .075 1.)
  (glColor4f 1. 1. 1. 1.)
  (image-render TX-LIFE-BAR)

  (glLoadIdentity)
  (glTranslatef 0. 1.425 0.)
  (glScalef 1. .075 1.)
  (image-render TX-LIFE-GROOVE)

  (glDisable GL_BLEND))

;; score

(define score-object #f)

(define (make-score-object)
  (make-2d-object
   font-perspective
   position: (make-vec3d 45. 447. 0.)
   scale: (make-vec3d 1. 1. 1.)
   color: (make-vec4d 1. 1. 1. 1.)
   font: (make-2d-font default-font50
                       (number->string (score))
                       35.)
   center: (make-vec3d 22. 16. 0.)))

(define (on-score-increase)
  (overlay-list-remove score-object)
  (overlay-list-add
   (make-tween
    score-object
    scale: (make-vec3d 2.5 2.5 2.5)
    alpha: 0.
    length: .5
    type: 'ease-out-cubic
    on-finished: (lambda () #f)))
  (set! score-object (make-score-object))
  (overlay-list-add score-object))

(define (score-setup)
  (set! score-object (make-score-object))
  (overlay-list-add score-object)
  #;(level-setup))

(define (score-remove)
  (if score-object
      (overlay-list-remove score-object)))

;; level

(define level-object #f)

(define (make-level-object)
  (make-2d-object
   font-perspective
   position: (make-vec3d 200. 447. 0.)
   font: (make-2d-font default-font50
                       (number->string (or *current-difficulty* 0))
                       35.)
   center: (make-vec3d 22. 16. 0.)))

(define (level-setup)
  (set! level-object (make-level-object))
  (overlay-list-add level-object))

(define (level-update)
  (if level-object
      (overlay-list-remove level-object))
  #;(level-setup))

;; overlay

(define (overlay-init)
  (set! TX-LIFE-GROOVE (image-opengl-load "life-groove.png"))
  (set! TX-LIFE-BAR (image-opengl-load "life-bar.png"))
  (set! TX-BUTTON (image-opengl-load "button.png")))
