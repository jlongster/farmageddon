;;;; standard-scene-objects
;;; Provides a set of scene objects which provide various
;;; functionality like fading

;; fader -- a "fader" is basically a new type of scene object which
;; takes a scene object and interpolates its alpha value over time.
;; The scene object is responsible for rendering itself with the
;; blending.

(define (make-faded #!optional
                    (length 2.)
                    scene-obj
                    (cont values))
  ;; The scene object system is busted; the following slots are used
  ;; to store the respective fader-specific data:
  ;; * color -> length
  ;; * position -> continuation
  ;; * rotation -> scene object reference
  (make-scene-object
   #f
   length
   cont
   (or scene-obj (black-polygon))
   #f
   #f
   #f
   fader-update
   fader-render))

(define (fader-length fader)
  ;; See comment in `make-fader`
  (scene-object-color fader))

(define (fader-cont fader)
  (scene-object-position fader))

(define (fader-scene-obj fader)
  (scene-object-rotation fader))

(define (fader-render fader)
  (if (not (scene-object-data fader))
      (scene-object-data-set! fader (real-time)))

  (let* ((length (fader-length fader))
         (status (- (real-time) (scene-object-data fader)))
         (scene-obj (fader-scene-obj fader))
         (color (scene-object-color scene-obj)))
    (vec4d-w-set! color (/ status length))
    (scene-object-render scene-obj)))

(define (fader-update fader)
  (let* ((length (fader-length fader))
         (started (scene-object-data fader))
         (status (and started (- (real-time) started))))
    (if (or (not status)
            (<= status length))
        #t
        (begin
          ((fader-cont fader))
          #f))))

;; util

(define (setup-2d-scene)
  (glMatrixMode GL_PROJECTION)
  (glLoadIdentity)
  (ortho 0.0 1.0 1.0 0.0 -1.0 1.0)
  (glMatrixMode GL_MODELVIEW)
  (glLoadIdentity))

(define (black-polygon value)
  (setup-2d-scene)
  (glColor4f 0. 0. 0. value)
  (glEnable GL_BLEND)
  (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
  (image-render-base)
  (glDisable GL_BLEND))

(define (render-faded-texture tex value)
  (glColor4f 0. 0. 0. value)
  (glEnable GL_BLEND)
  (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
  (image-render tex)
  (glDisable GL_BLEND))
