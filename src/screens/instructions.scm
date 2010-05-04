
(define instructions-texture #f)

(define (instructions-fade-out)
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

(define-screen instructions-screen
  init: (lambda ()
          (set! instructions-texture
                (image-opengl-load "instructions.png")))
  setup: (lambda ()
           (overlay-list-add
            (make-2d-object
             2d-perspective
             texture: instructions-texture
             scale: (make-vec3d (/ 512. 320.)
                                (/ 512. 480.)
                                1.)))
           (overlay-list-add
            (make-tween
             (make-2d-object
              2d-perspective
              color: (make-vec4d 0. 0. 0. 1.))
             length: .25
             alpha: 0.
             type: 'ease-in-cubic)
            important: #t))
  run: overlay-update
  render: overlay-render
  touches-began: (lambda (touches event)
                   (player-saw-instructions)
                   (instructions-fade-out)))
