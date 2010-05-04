
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

(define (overlay-add-button label pos width height on-click #!optional tex)
  (overlay-list-add
   (make-2d-object
    2d-perspective
    texture: (or tex TX-BUTTON)
    
    ;; We have to scale the image because we had to enlarge
    ;; it to be a power of 2 size, and the following
    ;; seemingly arbitrary scales were the proportions.
    ;; Previous size: 270x88
    ;; Current size: 512x128
    ;; Ratios: 512/270 and 128/88
    scale: (make-vec3d (* width 1.8962962963)
                       (* height .1 1.45454545)
                       1.)
    
    position: (make-vec3d
               (vec2d-x pos)
               (vec2d-y pos)
               0.))
   important: #t)

  (let* ((scn-width (UIView-width (current-view)))
         (scn-height (UIView-height (current-view)))
         (txt-width (* (ftgl-get-font-advance default-font50 label) .44 height))
         (btn-width (* scn-width width))
         (obj (make-2d-object
               font-perspective
               font: (make-2d-font default-font24 label (* 22. height))
               position: (make-vec3d
                          (+ (* (vec2d-x pos) scn-width)
                             (/ (- btn-width txt-width) 2.))
                          (* (- 1. (+ (vec2d-y pos) (* .065 height))) scn-height)
                          0.))))
    (overlay-list-add obj important: #t)
  
    (set! buttons
          (cons (make-button pos
                             (make-vec2d width (* .1 height))
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
