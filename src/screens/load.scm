
(declare (block)
         (standard-bindings)
         (extended-bindings))

(include "../lib/macros.scm")
(include "../config.scm")

(define font-perspective #f)

(define (set-loading-text! txt)
  (overlay-list-clear!)
  (add-centered-font default-font24 txt 40. 18.))

(define (loading-finished)
  (set-screen! title-screen))

(define-screen load-screen
  init: values
  setup: (lambda ()
           (set! title-texture
                 (expand-if LITE
                            (image-opengl-load "title-lite.png")
                            (image-opengl-load "title-screen.png")))

           (let ((width (UIView-width (current-view)))
                 (height (UIView-height (current-view))))
             (set! font-perspective
                   (ortho 0 (exact->inexact width)
                          0 (exact->inexact height)
                          -10000.0 10000.0)))

           (set! default-font24
                 (ftgl-create-texture-font (resource "ApexSansExtraBoldC.ttf")))
           
           (ftgl-set-font-face-size default-font24 24)
           (ftgl-get-font-advance
            default-font24
            "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789~-:/!.>")

           (scene-list-add
            (make-2d-object
             2d-perspective
             texture: title-texture))

           (set-loading-text! "LOADING...")
           (screen-run-initializers))
  run: (lambda ()
         (scene-list-update)
         (overlay-update))
  render: (lambda ()
            (scene-list-render)
            (overlay-render)))
