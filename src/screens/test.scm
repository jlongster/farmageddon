
(define-screen test-screen
  init: values
  setup: (lambda ()
           (scene-list-add
            (make-2d-object
             2d-perspective
             color: (make-vec4d 0. 1. 0. 1.))))
  run: scene-list-update
  render: scene-list-render)
