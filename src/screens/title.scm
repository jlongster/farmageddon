
(define title-texture #f)
(define play-texture #f)

(define (title-screen-touches-began touches event)
  (define (fade-out)
    (scene-list-add
     (make-tween
      (make-2d-object
       2d-perspective
       color: (make-vec4d 0. 0. 0. 0.))
      length: .5
      alpha: 1.
      type: 'ease-out-quad
      on-finished:
      (lambda ()
        (first-level)
        (set-screen! level-screen)
        #f))
     important: #t))

  (for-each
   (lambda (el)
     (let* ((width (UIView-width (current-view)))
            (height (UIView-height (current-view)))
            (loc (UITouch-location el))
            (x (/ (car loc) width))
            (y (* (/ (cdr loc) height) 1.5)))
       (if (and (> x .2) (< x .8)
                (> y 1.19) (< y 1.49))
           (fade-out))))
   touches))

(define-screen title-screen
  init: (lambda ()
          (set! title-texture
                (image-opengl-load "title-screen.png"))
          (set! play-texture
                (image-opengl-load "play.png")))
  setup: (lambda ()
           (scene-list-add
            (make-2d-object
             2d-perspective
             texture: title-texture))
           (scene-list-add
            (make-2d-object
             2d-ratio-perspective
             position: (make-vec3d .2 1.19 .3)
             scale: (make-vec3d .6 .3 1.)
             texture: play-texture)))
  run: scene-list-update
  render: scene-list-render
  touches-began: (lambda args (apply title-screen-touches-began args)))

