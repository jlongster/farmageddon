
(define-type button
  pos
  dimen
  handler
  obj)

(define buttons '())

(define (buttons-clear!)
  (set! buttons '()))

(define (add-button button)
  (set! buttons
        (cons button buttons)))

(define (overlay-add-fancy-button label pos on-click)
  (let* ((scn-width (UIView-width (current-view)))
         (scn-height (UIView-height (current-view)))
         (txt-width (* (ftgl-get-font-advance default-font24 label) .917))
         (btn-width 134.)
         (btn-height 45.)
         (obj (make-2d-object
               font-perspective
               font: (make-2d-font default-font24 label 22.)
               position: (make-vec3d
                          (+ (* (vec2d-x pos) scn-width)
                             (/ (- btn-width txt-width) 2.))
                          (- (* (- 1. (vec2d-y pos)) scn-height) (* btn-height .68))
                          0.))))
    (let ((scale (make-vec3d (exact->inexact (/ 134. scn-width))
                             (exact->inexact (/ 45. scn-height))
                             1.))
          (pos (make-vec3d (vec2d-x pos) (vec2d-y pos) 0.)))
      (overlay-list-add
       (make-2d-object
        2d-perspective
        texture: TX-BUTTON
        texture-scale: (if (high-res?)
                           (make-vec2d 1. 1.)
                           (make-vec2d (/ btn-width 256.)
                                       (/ btn-height 64)))
        scale: scale
        position: pos)
       important: #t))
    
    (overlay-list-add obj important: #t)
    
    (set! buttons
          (cons (make-button pos
                             (make-vec2d (exact->inexact (/ 134. scn-width))
                                         (exact->inexact (/ 45. scn-height)))
                             on-click
                             obj)
                buttons))))

(define (overlay-add-button label scale pos font-size on-click)
  (let* ((scn-width (UIView-width (current-view)))
         (scn-height (UIView-height (current-view)))
         (txt-width (* (ftgl-get-font-advance default-font24 label) (/ font-size 24.)))
         (btn-width (vec2d-x scale))
         (btn-height (vec2d-y scale))
         (obj (make-2d-object
               font-perspective
               font: (make-2d-font default-font24 label font-size)
               position: (make-vec3d
                          (+ (* (vec2d-x pos) scn-width)
                             (/ (- (* btn-width scn-width) txt-width) 2.))
                          (- (* (- 1. (vec2d-y pos)) scn-height)
                             (* (* btn-height scn-height) .76))
                          0.))))
    (let ((scale (make-vec3d (vec2d-x scale)
                             (vec2d-y scale)
                             1.))
          (pos (make-vec3d (vec2d-x pos) (vec2d-y pos) 0.)))
      (overlay-list-add
       (make-2d-object
        2d-perspective
        color: (make-vec4d .76 .239 .133 1.)
        scale: scale
        position: pos)
       important: #t))
    
    (overlay-list-add obj important: #t)
    
    (set! buttons
          (cons (make-button pos
                             scale
                             on-click
                             obj)
                buttons))))

(define-event-handler (touches-began touches event)
  (for-each
   (lambda (touch)
     (let* ((width (UIView-width (current-view)))
            (height (UIView-height (current-view)))
            (loc (UITouch-location touch))
            (t-x (/ (car loc) width))
            (t-y (/ (cdr loc) height)))
       (for-each
        (lambda (button)
          (let* ((pos (button-pos button))
                 (x1 (vec2d-x pos))
                 (y1 (vec2d-y pos))
                 (dimen (button-dimen button))
                 (x2 (+ (vec2d-x dimen) x1))
                 (y2 (+ (vec2d-y dimen) y1)))
            (if (and (> t-x x1) (< t-x x2)
                     (> t-y y1) (< t-y y2))
                ((button-handler button)
                 (2d-object-font (button-obj button))))))
        buttons)))
   touches))
