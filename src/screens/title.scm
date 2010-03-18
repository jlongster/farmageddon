
(define title-texture #f)
(define cow-texture #f)
(define pig-texture #f)
(define chicken-texture #f)

(define *explosion-thread* #f)

(define (start-explosion-events)
  (if (not *explosion-thread*)
      (begin
        (set! *explosion-thread*
              (make-thread
               (lambda ()
                 (let loop ()
                   (thread-sleep! (random-in-range 2. 3.))
                   (make-explosion (list-ref '(cow pig chicken)
                                             (random-integer 3)))
                   (loop)))))
        (thread-start! *explosion-thread*))))

(define (stop-explosion-events)
  (if *explosion-thread*
      (thread-terminate! *explosion-thread*)))

(define (make-explosion animal)
  (let ((source (make-audio-source explosion3-audio)))
    (play-and-release-audio source))
  
  (scene-list-add
   (make-tween
    (make-2d-object
     2d-ratio-perspective
     texture: (case animal
                ((cow) cow-texture)
                ((pig) pig-texture)
                ((chicken) chicken-texture))
     scale: (case animal
              ((cow) (make-vec3d .23 .23 1.))
               ((pig) (make-vec3d .17 .17 1.))
              ((chicken) (make-vec3d .13 .13 1.)))
     position: (make-vec3d .5 (random-in-range 1. 1.2) 1.)
     rotation: (make-vec4d 0. 0. 1. 0.)
     center: #t)
    position: (make-vec3d (list-ref '(-.2 1.2)
                                    (floor (/ (random-integer 100) 50)))
                          (random-in-range .5 1.) 1.)
    rotation: (make-vec4d 0. 0. 1. 360.)
    on-finished: (lambda () #f))))

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
          (set! title-texture (image-opengl-load "title-screen.png"))
          (set! cow-texture (image-opengl-load "cow.png"))
          (set! pig-texture (image-opengl-load "pig.png"))
          (set! chicken-texture (image-opengl-load "chicken.png")))
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
                               (lambda ()
                                 (set-screen! scores-screen)))
           (start-explosion-events)
           (play-and-release-audio (make-audio-source explosion1-audio)))
  run: (lambda ()
         (scene-list-update)
         (overlay-update))
  render: (lambda ()
            (scene-list-render)
            (overlay-render)))

