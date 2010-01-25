;;;; weapons
;;; This code implements the logic and rendering functionality of
;;; weapons.

;; init

(define lightning-textures '())
(define star-texture #f)

(define (weapons-init)
  (let ((line (CGImageRef-load "line.png"))
        (line2 (CGImageRef-load "line2.png"))
        (line3 (CGImageRef-load "line3.png"))
        (star (CGImageRef-load "star.png")))
    (set! lightning-textures
          (list (image-opengl-upload
                 (CGImageRef-data line)
                 (CGImageRef-width line)
                 (CGImageRef-height line))
                (image-opengl-upload
                 (CGImageRef-data line2)
                 (CGImageRef-width line2)
                 (CGImageRef-height line2))
                (image-opengl-upload
                 (CGImageRef-data line3)
                 (CGImageRef-width line3)
                 (CGImageRef-height line3))))
    (set! star-texture (image-opengl-upload
                        (CGImageRef-data star)
                        (CGImageRef-width star)
                        (CGImageRef-height star)))))

;; creating

(define %%lightnings '())

(define-type lightning
  id: A4DC9FC6-AB12-46EC-ACBC-AE8837167B30
  constructor: really-make-lightning
  point
  angle
  texture
  scatter-vectors
  lifetime
  created-time)

(define %%last-texture-index 0)

(define (make-lightning point)
  (set! %%last-texture-index
        (remainder (+ %%last-texture-index 1)
                   (length lightning-textures)))
  
  (really-make-lightning
   point
   (* (spread-number (random-real)) 10.)
   (list-ref lightning-textures %%last-texture-index)
   (let ((count (+ 3 (random-integer 5))))
     (unfold (lambda (i) (>= i count))
             (lambda (i) (vec3d-unit
                          (make-vec3d (spread-number (random-real))
                                      (spread-number (random-real))
                                      (spread-number (random-real)))))
             (lambda (i) (+ i 1))
             0))
   5.
   (real-time)))

(define (add-lightning lightning)
  (set! %%lightnings (cons lightning %%lightnings))

  ;; Play the explosion sound
  ;; (explosion-source (make-audio-source explosion-audio))
  ;; (alSourcef explosion-source AL_GAIN 1.)
  ;; Play the explosion sound, and free it
  ;;(play-audio explosion-source)
  ;; Ewwww, look at that ugly hack!
  ;; (thread-start!
  ;;  (make-thread
  ;;   (lambda ()
  ;;     (thread-sleep! 1.7)
  ;;     (free-audio-source explosion-source))))
  )

(define (add-hit-point point)
  (add-lightning (make-lightning point)))

;; rendering

(define (render-lightning lightning)
  (let* ((now (real-time))
         (passed (- now (lightning-created-time lightning))))
    (if (>= passed (lightning-lifetime lightning))
        #f
        (%%render-lightning (lightning-point lightning)
                            (lightning-angle lightning)
                            (lightning-texture lightning)
                            passed
                            (/ passed (lightning-lifetime lightning))))))

(define (%%render-lightning point angle texture time-passed time-interp)
  (let* ((width (UIView-width (current-view)))
         (height (UIView-height (current-view)))
         (pers (current-perspective))
         (x (exact->inexact (* (/ (car point) width)
                               (perspective-xmax pers))))
         (y (exact->inexact (* (/ (cdr point) height)
                               (perspective-ymin pers)))))

    ;; Freaking alpha-premultiplication that Cocoa does
    ;; automatically. This simply allows me to fade out a
    ;; textured polygon, which is broken due to Apple's
    ;; crappy premultiplication which is forced on you.
    ;; Basically, I premultiply the fade out into the
    ;; color values using glColor4f since the texture
    ;; environment is set to GL_MODULATE.
    (glEnable GL_BLEND)
    (glBlendFunc GL_ONE GL_ONE_MINUS_SRC_ALPHA)
    (let ((fade (- 1. time-interp)))
      (glColor4f fade fade fade fade))

    ;; Render the lightning
    (let loop ((i 0))
      (if (< i 1)
          (begin
            (glLoadIdentity)
            (glTranslatef x y 0.)
            (glRotatef (+ 180.
                          angle)
                       0. 0. 1.)
            (glTranslatef -.2 0. 0.)
            (glScalef .4 1.5 1.)
            (image-render texture)
            (loop (+ i 1)))))

    ;; Render the "dust"
    (glLoadIdentity)
    (let* ((size .05)
           (half-size (/ size 2)))
      (glTranslatef x y 0.)
      (glRotatef (* time-passed 180) 0. 0. 1.)
      (glTranslatef (- half-size) (- half-size) 0.)
      (glScalef size size 1.))
    (image-render star-texture)

    (glDisable GL_BLEND)
    #t))

(define (render-lightnings)
  (set! %%lightnings
        (reverse
         (fold (lambda (lightning acc)
                 (let ((result (render-lightning lightning)))
                   (if result
                       (cons lightning acc)
                       acc)))
               '()
               %%lightnings))))

(define (render-weapons)
  (render-lightnings))
