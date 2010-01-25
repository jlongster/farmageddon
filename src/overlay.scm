;;;; overlay 
;;; Controls and renders all of the 2d components

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
  (glLoadIdentity)
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

(define (goal-render)
  (let ((left (goal-left)))
    (let loop ((i 0))
      (if (< i left)
          (begin
            (glLoadIdentity)
            (glEnable GL_BLEND)
            (glBlendFunc GL_ONE GL_ONE_MINUS_SRC_ALPHA)
            (glTranslatef (exact->inexact (* i .1)) 0. 0.)
            (glScalef .1 (/ .1 1.5) 1.)
            
            (image-render star-texture)
            
            (glDisable GL_BLEND)
            (loop (+ i 1)))))))

;; overlay

(define (overlay-init)
  (life-init))

(define (overlay-render)
  (life-render)
  (goal-render))
