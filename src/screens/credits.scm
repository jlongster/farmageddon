
(define (add-credit title name y)
  (overlay-list-add
   (make-2d-object
    font-perspective
    font: (make-2d-font default-font24 title 20.)
    position: (to-font-space .05 y)))

  (overlay-list-add
   (make-2d-object
    font-perspective
    font: (make-2d-font default-font24 name 20.)
    position: (to-font-space .52 y))))

(define-screen credits-screen
  init: values
  setup: (lambda ()           
           (overlay-list-add
            (make-2d-object
             2d-ratio-perspective
             texture: logo-texture
             scale: (make-vec3d 1. (/ 118. 480.) 1.)))

           (add-centered-font default-font50 "CREDITS" 360. 28.)

           (add-credit "DEVELOPED BY" "James Long" .4)
           (add-credit "3D DESIGN" "James Long" .47)
           (add-credit "2D DESIGN" "Young Monster" .54)
           (add-centered-font default-font24 "HTTP://FARMAGEDDONGAME.COM" 100. 18.)

           (overlay-add-button "BACK"
                               (make-vec2d .25 .85)
                               .5 1.
                               (lambda (this)
                                 (set-screen! title-screen))))
  run: overlay-update
  render: overlay-render)
