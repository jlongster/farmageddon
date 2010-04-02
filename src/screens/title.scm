
(define title-texture #f)
(define cow-texture #f)
(define pig-texture #f)
(define chicken-texture #f)
(define round-btn-texture #f)

(define *explosion-thread* #f)

(define (start-explosion-events)
  (if (not *explosion-thread*)
      (begin
        (set! *explosion-thread*
              (make-thread
               (lambda ()
                 (thread-sleep! 6.)
                 (let loop ()
                   (make-explosion)
                   (thread-sleep! (random-in-range 2.5 4.0))
                   (loop)))))
        (thread-start! *explosion-thread*))))

(define (stop-explosion-events)
  (if *explosion-thread*
      (begin
        (thread-terminate! *explosion-thread*)
        (set! *explosion-thread* #f))))

(define (make-explosion)
  (let ((source (make-audio-source explosion3-audio)))
    (alSourcef source AL_GAIN 1.)
    (play-and-release-audio source))

  (let ((animal (list-ref '(cow pig chicken)
                          (random-integer 3))))
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
      on-finished: (lambda () #f)))))

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

(define (add-info-button)
  (let ((obj (make-2d-object
              font-perspective
              font: (make-2d-font thin-font50 "?" 15.)
              position: (to-font-space .962 .983))))
    (overlay-list-add obj important: #t)
    (add-button
     (make-button (make-vec2d .93 .93)
                  (make-vec2d .1 .1)
                  (lambda (this)
                    (set-screen! credits-screen))
                  obj))))

(define *first-run* #t)

(define-screen title-screen
  init: (lambda ()
          (set! title-texture (image-opengl-load "title-screen.png"))
          (set! cow-texture (image-opengl-load "cow.png"))
          (set! pig-texture (image-opengl-load "pig.png"))
          (set! chicken-texture (image-opengl-load "chicken.png"))
          (set! round-btn-texture (image-opengl-load "round-button.png"))

          (if (not (read-sound))
              (mute-audio)))
  setup: (lambda ()
           (define (get-sound-text)
             (if (is-audio-muted?) "SOUND: OFF" "SOUND: ON")) 

           (scene-list-add
            (make-2d-object
             2d-perspective
             texture: title-texture))

           (overlay-add-button "PLAY" (make-vec2d .35 .79) .3 1.
                               (lambda (this)
                                 (stop-explosion-events)
                                 (fade-out)))
           (overlay-add-button (get-sound-text) (make-vec2d .1 .9) .4 .7
                               (lambda (this)
                                 (if (is-audio-muted?)
                                     (begin
                                       (unmute-audio)
                                       (2d-font-text-set! this (get-sound-text)))
                                     (begin
                                       (mute-audio)
                                       (2d-font-text-set! this (get-sound-text))))
                                 (save-sound)))
           (overlay-add-button "SCORES" (make-vec2d .5 .9) .4 .7
                               (lambda (this)
                                 (stop-explosion-events)
                                 (set-screen! scores-screen)))
           (add-info-button)
           (start-explosion-events)
           (set! *first-run* #t))
  run: (lambda ()
         (scene-list-update)
         (overlay-update)
         (if *first-run*
             (begin
               (play-and-release-audio
                (make-audio-source explosion1-audio))
               (make-explosion)
               (make-explosion)
               (make-explosion)
               (set! *first-run* #f))))
  render: (lambda ()
            (scene-list-render)
            (overlay-render)))

