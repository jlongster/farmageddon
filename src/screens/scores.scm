
(define-screen scores-screen
  init: values
  setup: (lambda ()

           (add-centered-font default-font50 "HIGH SCORES" 410. 30.)
           
           (let loop ((scores (get-high-scores))
                      (i 0))
             (if (not (null? scores))
                 (let ((score (car scores)))
                   (overlay-list-add
                    (make-2d-object
                     font-perspective
                     font: (make-2d-font default-font50
                                         (persistent-score-name score)
                                         20.)
                     position: (to-font-space .3 (+ .25 (* i .05)))))
                   
                   (overlay-list-add
                    (make-2d-object
                     font-perspective
                     font: (make-2d-font default-font50
                                         (number->string
                                          (persistent-score-score score))
                                         20.)
                     position: (to-font-space .6 (+ .25 (* i .05)))))
                   
                   (loop (cdr scores)
                         (+ i 1)))))
           
           (overlay-add-button "BACK"
                               (make-vec2d .25 .75)
                               .5 1.
                               (lambda ()
                                 (set-screen! title-screen))))
  run: overlay-update
  render: overlay-render)
