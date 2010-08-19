;;;; tween
;;; Implements a system for attaching a "tween" to a scene object
;;; which interpolates any of its attributes over time into the given
;;; values (thing morphing color, size, position, etc. over time).
;;;
;;; Supports various interpolation methods given by the "type" keyword
;;; parameter to `make-tween`:
;;;
;;; linear
;;; ease-in-quad
;;; ease-out-quad
;;; ease-inout-quad
;;; ease-in-cubic
;;; ease-out-cubic
;;; ease-inout-cubic
;;;

(declare (block)
         (standard-bindings)
         (extended-bindings))

(define (make-tween scene-obj
                    #!key
                    (length 2.)
                    color
                    alpha
                    scale
                    position
                    local-position
                    rotation
                    (render #t)
                    (type 'linear)
                    (on-finished values))
  (let* ((started-time (real-time))
         (src-color (and-let* ((vec (generic-object-color scene-obj)))
                      (vec4d-copy vec)))
         (src-alpha (and src-color (vec4d-w src-color)))
         (src-scale (and-let* ((vec (generic-object-scale scene-obj)))
                      (vec3d-copy vec)))
         (src-position (and-let* ((vec (generic-object-position scene-obj)))
                         (vec3d-copy vec)))
         (src-local-position (and-let* ((vec (generic-object-local-position scene-obj)))
                               (vec4d-copy vec)))
         (src-rotation (and-let* ((vec (generic-object-rotation scene-obj)))
                         (vec4d-copy vec))))
    (make-scene-object
     (generic-object-perspective scene-obj)
     (lambda (fader)
       (if render
           (render-generic-object scene-obj)))
     (lambda (fader)
       (and (update-generic-object scene-obj)
            (let* ((time-span (- (real-time) started-time))
                   (interp (/ time-span length)))
              (if (> time-span length)
                  (set! interp 1.))
              
              (if color
                  (tween-color scene-obj
                               type
                               interp
                               src-color
                               color))
              (if alpha
                  (tween-alpha scene-obj
                               type
                               interp
                               src-alpha
                               alpha))
              (if scale
                  (tween-scale scene-obj
                               type
                               interp
                               src-scale
                               scale))
              (if position
                  (tween-position scene-obj
                                  type
                                  interp
                                  src-position
                                  position))
              (if local-position
                  (tween-local-position scene-obj
                                        type
                                        interp
                                        src-local-position
                                        local-position))
              (if rotation
                  (tween-rotation scene-obj
                                  type
                                  interp
                                  src-rotation
                                  rotation))
           
              (if (> time-span length)
                  (if (not (on-finished))
                      (scene-list-remove fader))))))
     scene-obj)))

;; types of interpolation

(define interpolations (make-table))

(define-macro (install-interpolation name code)
  `(table-set! interpolations ',name ,code))

(define (get-interpolation name)
  (table-ref interpolations name #f))

(install-interpolation linear
  (lambda (interp src-value value)
    (+ src-value (* interp (- value src-value)))))

(install-interpolation ease-in-quad
  (lambda (interp src-value value)
    (+ src-value (* interp interp (- value src-value)))))

(install-interpolation ease-out-quad
  (lambda (interp src-value value)
    (+ src-value (* interp
                    (- interp 2.)
                    (- (- value src-value))))))

(install-interpolation ease-inout-quad
  (lambda (interp src-value value)
    (let ((half-change (/ (- value src-value) 2.)))
      (if (< interp .5)
          (let ((interp (* interp 2.)))
            (+ src-value (* interp interp half-change)))
          (let ((interp (- (* interp 2.) 1.)))
            (+ src-value (* (- (* interp
                                  (- interp 2.))
                               1.)
                            (- half-change))))))))

(install-interpolation ease-in-cubic
  (lambda (interp src-value value)
    (let ((interp (* interp interp interp)))
      (+ src-value (* interp (- value src-value))))))

(install-interpolation ease-out-cubic
  (lambda (interp src-value value)
    (let ((interp (+ (expt (- interp 1.) 3) 1.)))
      (+ src-value (* interp (- value src-value))))))

(install-interpolation ease-inout-cubic
  (lambda (interp src-value value)
    (let ((half-change (/ (- value src-value) 2.)))
      (if (< interp .5)
          (let ((interp (* interp 2.)))
            (+ src-value (* interp interp interp half-change)))
          (let ((interp (- (* interp 2.) 1.)))
            (+ src-value (* (+ (expt (- interp 1.) 3)
                               2.)
                            half-change)))))))

(install-interpolation ease-out-bounce
  (lambda (interp src-value value)
    (let ((change (- value src-value)))
      (cond
       ((< interp (/ 2.75))
        (+ src-value
           (* 7.5625 interp interp change)))
       ((< interp (/ 2 2.75))
        (let ((interp (- interp (/ 1.5 2.75))))
          (+ src-value
             (* (+ (* 7.5625 interp interp) .75)
                change))))
       ((< interp (/ 2.5 2.75))
        (let ((interp (- interp (/ 2.25 2.75))))
          (+ src-value
             (* (+ (* 7.5625 interp interp) .9375)
                change))))
       (else
        (let ((interp (- interp (/ 2.625 2.75))))
          (+ src-value
             (* (+ (* 7.5625 interp interp) .984375)
                change))))))))

(install-interpolation ease-in-bounce
  (lambda (interp src-value value)
    (let ((change (- value src-value)))
      (+ src-value
         change
         (- ((get-interpolation 'ease-out-bounce)
             (- 1. interp) 0 change))))))

(install-interpolation ease-inout-bounce
  (lambda (interp src-value value)
    (let ((change (- value src-value)))
      (if (< interp .5)
          (+ src-value
             (* ((get-interpolation 'ease-in-bounce)
                 (* interp 2.) 0. change)
                .5))
          (+ src-value
             (* change .5)
             (* ((get-interpolation 'ease-out-bounce)
                 (* (- interp .5) 2.) 0. change)
                .5))))))

;; attribute tweening

(define (tween type interp src-value value)
  (let ((interpolater (get-interpolation type)))
    (if interpolater
        (interpolater interp src-value value)
        (error "Invalid tweening type" type))))

(define (tween-color scene-obj type interp src-color color)
  (let ((vec (generic-object-color scene-obj)))
    (vec4d-x-set! vec (tween type interp
                             (vec4d-x src-color) (vec4d-x color)))
    (vec4d-y-set! vec (tween type interp
                             (vec4d-y src-color) (vec4d-y color)))
    (vec4d-z-set! vec (tween type interp
                             (vec4d-z src-color) (vec4d-z color)))
    (vec4d-w-set! vec (tween type interp
                             (vec4d-w src-color) (vec4d-w color)))))

(define (tween-alpha scene-obj type interp src-alpha alpha)
  (let ((vec (generic-object-color scene-obj)))
    (vec4d-w-set! vec (tween type interp src-alpha alpha))))

(define (tween-scale scene-obj type interp src-scale scale)
  (let ((vec (generic-object-scale scene-obj)))
    (vec3d-x-set! vec (tween type interp
                             (vec3d-x src-scale) (vec4d-x scale)))
    (vec3d-y-set! vec (tween type interp
                             (vec3d-y src-scale) (vec4d-y scale)))
    (vec3d-z-set! vec (tween type interp
                             (vec3d-z src-scale) (vec4d-z scale)))))

(define (tween-position scene-obj type interp src-position position)
  (let ((vec (generic-object-position scene-obj)))
    (vec3d-x-set! vec (tween type interp
                             (vec3d-x src-position) (vec4d-x position)))
    (vec3d-y-set! vec (tween type interp
                             (vec3d-y src-position) (vec4d-y position)))
    (vec3d-z-set! vec (tween type interp
                             (vec3d-z src-position) (vec4d-z position)))))

(define (tween-local-position scene-obj type interp src-position position)
  (let ((vec (generic-object-local-position scene-obj)))
    (vec3d-x-set! vec (tween type interp
                             (vec3d-x src-position) (vec4d-x position)))
    (vec3d-y-set! vec (tween type interp
                             (vec3d-y src-position) (vec4d-y position)))
    (vec3d-z-set! vec (tween type interp
                             (vec3d-z src-position) (vec4d-z position)))))

(define (tween-rotation scene-obj type interp src-rotation rotation)
  (let ((vec (generic-object-rotation scene-obj)))
    (vec4d-x-set! vec (tween type interp
                             (vec4d-x src-rotation) (vec4d-x rotation)))
    (vec4d-y-set! vec (tween type interp
                             (vec4d-y src-rotation) (vec4d-y rotation)))
    (vec4d-z-set! vec (tween type interp
                             (vec4d-z src-rotation) (vec4d-z rotation)))
    (vec4d-w-set! vec (tween type interp
                             (vec4d-w src-rotation) (vec4d-w rotation)))))

