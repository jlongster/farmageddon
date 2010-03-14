
(define title-texture #f)
(define play-texture #f)

(define (fade-out)
  (overlay-list-add
   (make-tween
    (make-2d-object
     2d-perspective
     color: (make-vec4d 0. 0. 0. 0.))
    length: .5
    alpha: 1.
    type: 'ease-out-quad
    on-finished:
    (lambda ()
      (set-screen! level-screen)
      #f))
   important: #t))

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

           (overlay-add-button "PLAY" (make-vec2d .35 .79) .3 1.
                               fade-out)
           (overlay-add-button "SOUND: OFF" (make-vec2d .1 .9) .4 .7
                               values)
           (overlay-add-button "SCORES" (make-vec2d .5 .9) .4 .7
                               values))
  run: (lambda ()
         (scene-list-update)
         (overlay-update))
  render: (lambda ()
            (scene-list-render)
            (overlay-render)))

