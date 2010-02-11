;;;; test screen
;;; A bed for playing around with various parts of the system

(define sheep-mesh (obj-load (resource "sheep") #t))
(define bg #f)

(define (render-test-screen)
  (load-perspective 2d-perspective)
  (glColor4f 1. 1. 1. 1.)
  (if bg
      (image-render bg))
  (scene-list-render))

(define-screen test-screen
  init: values
  run: (lambda ()
         (scene-list-update))
  render: render-test-screen)
