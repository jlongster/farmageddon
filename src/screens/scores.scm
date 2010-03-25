
(define logo-texture #f)

(define-screen scores-screen
  init: (lambda ()
          (set! logo-texture (image-opengl-load "logo.png")))
  setup: (lambda ()

           (overlay-list-add
            (make-2d-object
             2d-ratio-perspective
             texture: logo-texture
             scale: (make-vec3d 1. (/ 118. 480.) 1.)))
           
           (add-centered-font default-font50 "HIGH SCORES" 360. 28.)
           
           (let loop ((scores (get-high-scores))
                      (i 0))
             (if (not (null? scores))
                 (let ((score (car scores)))
                   (overlay-list-add
                    (make-2d-object
                     font-perspective
                     font: (make-2d-font thin-font50
                                         (persistent-score-name score)
                                         24.)
                     position: (to-font-space .25 (+ .33 (* i .05)))))
                   
                   (overlay-list-add
                    (make-2d-object
                     font-perspective
                     font: (make-2d-font thin-font50
                                         (number->string
                                          (persistent-score-score score))
                                         20.)
                     position: (to-font-space .6 (+ .33 (* i .05)))))
                   
                   (loop (cdr scores)
                         (+ i 1)))))
           
           (overlay-add-button "BACK"
                               (make-vec2d .25 .85)
                               .5 1.
                               (lambda (this)
                                 (set-screen! title-screen))))
  run: overlay-update
  render: overlay-render)
