
(define button-texture #f)

(define-screen button-screen
  init: (lambda ()
         (set! button-texture
                (image-opengl-load "button.png")))
  setup: (lambda ()
             (overlay-add-button "TRY AGAIN" (make-vec2d .25 .3) 1.
                      values)
             (overlay-add-button "MENU" (make-vec2d .25 .5) 1.
                                 values)
             
             (overlay-add-button "PLAY" (make-vec2d .25 .7) .7
                                 (lambda ()
                                   (overlay-add-button "GOT" (make-vec2d .25 .1) 1. values))))
  run: overlay-update
  render: overlay-render)
