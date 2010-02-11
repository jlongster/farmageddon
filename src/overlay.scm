;;;; overlay 
;;; Controls and renders all of the 2d components

;; custom scene list

(define overlay-list '())

(define (overlay-list-clear!)
  (set! overlay-list '()))

(define (overlay-list-add obj)
  (set! overlay-list (cons obj overlay-list)))

(define (overlay-render)
  (scene-list-render #f overlay-list)

  (load-perspective 2d-ratio-perspective)
  
  (life-render)
  (goal-render))

(define (overlay-update)
  (set! overlay-list
        (scene-list-update #f #f overlay-list)))

;; life

(define TX-LIFE-GROOVE #f)
(define TX-LIFE-BAR #f)

(define (life-init)
  (let ((image (CGImageRef-load "life-groove.png")))
    (set! TX-LIFE-GROOVE (image-opengl-upload
                         (CGImageRef-data image)
                         (CGImageRef-width image)
                         (CGImageRef-height image))))

  (let ((image (CGImageRef-load "life-bar.png")))
    (set! TX-LIFE-BAR (image-opengl-upload
                       (CGImageRef-data image)
                       (CGImageRef-width image)
                       (CGImageRef-height image)))))

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

;; goal (based on score)

(define goal-font #f)

(define (goal-init)
  (set! goal-font
        (make-2d-font apex-font
                      (number->string (goal-left))
                      72))
  
  (overlay-list-add
   (make-2d-object
    font-perspective
    position: (make-vec3d 30. 30. 0.)
    font: goal-font)))

(define (goal-render)
  (2d-font-text-set! goal-font
                     (number->string (goal-left))))

;; overlay

(define (overlay-init)
  (life-init)
  (goal-init))
