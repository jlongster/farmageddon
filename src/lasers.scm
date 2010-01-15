
;; lasers

(define %%lasers '())

(define (render-laser deg point then now lifetime)
  (let* ((width (UIView-width (current-view)))
         (height (UIView-height (current-view)))
         (pers (current-perspective))
         (x (exact->inexact (* (/ (car point) width)
                               (perspective-xmax pers))))
         (y (exact->inexact (* (/ (cdr point) height)
                               (perspective-ymin pers)))))
    (glLoadIdentity)
    (glTranslatef x y 0.)
    (glRotatef deg 0. 0. 1.)
    (glTranslatef 0. -.05 0.)
    (glScalef 1.5 .07 1.)

    ;; Freaking alpha-premultiplication that Cocoa does
    ;; automatically. This simply allows me to fade out a
    ;; textured polygon, which is broken due to Apple's
    ;; crappy premultiplication which is forced on you.
    ;; Basically, I premultiply the fade out into the
    ;; color values using glColor4f since the texture
    ;; environment is set to GL_MODULATE.
    (glEnable GL_BLEND)
    (glBlendFunc GL_ONE GL_ONE_MINUS_SRC_ALPHA)
    (let ((fade (- 1. (/ (- now then)
                         lifetime))))
      (glColor4f fade fade fade fade))
          
    (image-render (case (random-integer 2)
                    ((0) line-texture)
                    ((1) line2-texture)))

    (glLoadIdentity)
    (let* ((size .1)
           (half-size (/ size 2)))
      (glTranslatef (- x half-size)
                    (- y half-size)
                    0.)
      (glScalef size size 1.))
    (image-render star-texture)
    
    (glDisable GL_BLEND)
    #t))

(define (add-laser deg point lifetime)
  (let ((then (real-time))
        (explosion-source (make-audio-source explosion-audio)))
    (alSourcef explosion-source AL_GAIN 1.)
    ;; Play the explosion sound, and free it
    (play-audio explosion-source)
    ;; Ewwww, look at that ugly hack!
    (thread-start!
     (make-thread
      (lambda ()
        (thread-sleep! 1.7)
        (free-audio-source explosion-source))))
    
    (set!
     %%lasers
     (cons 
      (lambda ()
        (let ((now (real-time)))
          (if (>= (- now then) lifetime)
              #f
              (render-laser deg point then now lifetime))))
      %%lasers))))

(define (make-laser point)
  (define (l)
    (* (random-real) .4))
  
  (let ((deg (+ -90. (* (spread-number (random-real)) 40.))))
    (add-laser (+ deg (* (spread-number (random-real)) 15.))
               point
               (l))
    (add-laser deg
               point
               (l))))

(define (render-lasers)
  (set! %%lasers
        (reverse
         (fold (lambda (render-laser acc)
                 (let ((result (render-laser)))
                   (if result
                       (cons render-laser acc)
                       acc)))
               '()
               %%lasers))))
