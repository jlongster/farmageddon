
(define level-name-texture #f)
(define name-font #f)
(define big-name-font #f)



(define (level-add-instructions)
  (case (current-goal-type)
    ((goal)
     (add-centered-font name-font
                        "Kill "
                        235.)
     (add-centered-font big-name-font
                        (number->string (current-level-goal))
                        180.)
     (add-centered-font name-font
                        "animals"
                        150.))
    ((timer)
     (add-centered-font name-font
                        "Stay alive for "
                        170.)
     (add-centered-font name-font
                        (string-append
                         (number->string (current-level-goal))
                         " seconds")
                        145.))))

(define-screen level-name-screen
  init: (lambda ()
          (set! level-name-texture
                (image-opengl-load "level-name-bg.png"))
          (set! name-font
                (ftgl-create-texture-font (resource "ApexSansExtraBoldC.ttf")))
          (set! big-name-font
                (ftgl-create-texture-font (resource "ApexSansExtraBoldC.ttf")))
          (ftgl-set-font-face-size name-font 24)
          (ftgl-set-font-face-size big-name-font 50))
  setup: (lambda ()
           ;; background
           (overlay-list-add
            (make-2d-object
             2d-perspective
             texture: level-name-texture))

           ;; text
           (let* ((title (current-level-name))
                  (width (UIView-width (current-view)))
                  (font (if (number? title)
                            (make-2d-font big-name-font
                                          (number->string title)
                                          50)
                            (make-2d-font name-font title 24))))
             (overlay-list-add
              (make-2d-object
               font-perspective
               font: font
               position: (make-vec3d (/ width 2.) 340. 0.)
               center: (make-vec3d (/ (ftgl-get-font-advance
                                       (2d-font-font font)
                                       (2d-font-text font))
                                      2.)
                                   0.
                                   0.))
              important: #t)

             (level-add-instructions))
           
           ;; fade in and out
           (let ((time (+ (real-time) 2.)))
             (overlay-list-add
              (make-tween
               (make-2d-object
                2d-perspective
                color: (make-vec4d 0. 0. 0. 1.))
               length: 2.
               alpha: 0.
               type: 'ease-out-cubic
               on-finished:
               (lambda ()
                 (if (> (real-time) time)
                     (begin
                       (overlay-list-add
                        (make-tween
                         (make-2d-object
                          2d-perspective
                          color: (make-vec4d 0. 0. 0. 0.))
                         length: .5
                         alpha: 1.
                         on-finished: (lambda ()
                                        (set-screen! level-screen)))
                        important: #t)))))
              important: #t)))
  run: overlay-update
  render: overlay-render)
