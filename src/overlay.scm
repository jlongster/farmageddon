;;;; overlay 
;;; Controls and renders all of the 2d components

(declare (block)
         (standard-bindings)
         (extended-bindings))

;; custom scene list

(define overlay-list '())

(define (overlay-list-clear!)
  (set! overlay-list '()))

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

(define goal-object #f)

(define (make-goal-object)
  (make-2d-object
   font-perspective
   position: (make-vec3d 45. 438. 0.)
   scale: (make-vec3d 1. 1. 1.)
   color: (make-vec4d 1. 1. 1. 1.)
   font: (make-2d-font big-name-font
                       (number->string (goal-left))
                       50)
   center: (make-vec3d 22. 20. 0.)))

(define (on-score-increase)
  (if (eq? (current-goal-type) 'goal)
      (begin
        (overlay-list-remove goal-object)
        (overlay-list-add
         (make-tween
          goal-object
          scale: (make-vec3d 2.5 2.5 2.5)
          alpha: 0.
          length: .5
          type: 'ease-out-cubic
          on-finished: (lambda () #f)))
        (set! goal-object (make-goal-object))
        (overlay-list-add goal-object))))

(define (goal-setup)
  (set! goal-object (make-goal-object))
  (overlay-list-add goal-object))

;; timer

(define (make-timer-object)
  (make-2d-object
   font-perspective
   position: (make-vec3d 118. 438. 0.)
   scale: (make-vec3d 1. 1. 1.)
   color: (make-vec4d 1. 1. 1. 1.)
   font: (make-2d-font big-name-font
                       ""
                       50)
   center: (make-vec3d 22. 20. 0.)))

(define (timer-setup)
  (let ((font-obj (make-timer-object)))
    (overlay-list-add
     (make-scene-object
      (generic-object-perspective font-obj)
      (lambda (obj)
        (render-generic-object font-obj))
      (lambda (obj)
        (and (update-generic-object font-obj)
             (let ((font (2d-object-font font-obj))
                   (t (timer-status)))
               (if (not (player-finished?))
                   (2d-font-text-set!
                    font
                    (string-append "00:"
                                   (if (< t 10) "0" "")
                                   (number->string t)))))))))))

;; overlay

(define (overlay-init)
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

(define (overlay-setup)
  (case (current-goal-type)
    ((goal) (goal-setup))
    (else (timer-setup))))
