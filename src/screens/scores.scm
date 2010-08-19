
(declare (block)
         (standard-bindings)
         (extended-bindings))

(define logo-texture #f)
(define *global-score-obj* #f)
(define *loading-obj* #f)

(define (update-global-score n)
  (let ((font (2d-object-font *global-score-obj*)))
    (2d-font-text-set! font (number->string n))))

(define (add-feint-button)
    (let ((obj (make-2d-object
                font-perspective
                font: (make-2d-font default-font24 "FEINT >" 14.)
                position: (to-font-space .8 .985))))
      (overlay-list-add obj important: #t)
      (add-button
       (make-button (make-vec2d .7 .94)
                    (make-vec2d .3 .1)
                    (lambda (this)
                      (feint-open-dashboard))
                    obj))))

(define (load-default-screen)
  (overlay-list-clear!)
  (hide-info-button)
  
  (overlay-list-add
   (make-2d-object
    2d-ratio-perspective
    texture: logo-texture
    scale: (make-vec3d 1. (/ 118. 480.) 1.)))

  (add-centered-font default-font24 "ALL-TIME" 370. 24.)

  (overlay-list-add
   (make-2d-object
    font-perspective
    font: (make-2d-font default-font24 "LOCAL" 20.)
    position: (to-font-space .1 .285)
    color: (make-vec4d 1. .3 .3 1.)))

  (overlay-list-add
   (make-2d-object
    font-perspective
    font: (make-2d-font default-font24
                        (number->string (get-highest-score))
                        20.)
    position: (to-font-space .1 .34)))

  (overlay-list-add
   (make-2d-object
    font-perspective
    font: (make-2d-font default-font24 "GLOBAL" 20.)
    position: (to-font-space .6 .285)
    color: (make-vec4d .3 .3 1. 1.)))
  
  (overlay-list-add *global-score-obj*)

  (overlay-add-fancy-button "BACK"
                            (make-vec2d (+ .09 *button-width*) .855)
                            (lambda (this)
                              (set-screen! title-screen)))

  (add-feint-button))

(define (shorten-name name)
  (let ((len (string-length name)))
    (if (> len 13)
        (string-append (string-copy name 0 8)
                       "..."
                       (string-copy name (- len 4) len))
        name)))

(define (show-scores scores color)
  (let loop ((scores scores)
             (i 0))
    (if (and (not (null? scores))
             (< i 8))
        (let ((score (car scores)))
          (overlay-list-add
           (make-2d-object
            font-perspective
            font: (make-2d-font default-font24
                                (shorten-name (persistent-score-name score))
                                22.)
            position: (to-font-space .1 (+ .49 (* i .05)))
            color: color))
          
          (overlay-list-add
           (make-2d-object
            font-perspective
            font: (make-2d-font default-font24
                                (number->string
                                 (persistent-score-score score))
                                20.)
            position: (to-font-space .64 (+ .49 (* i .05)))))
          
          (loop (cdr scores)
                (+ i 1))))))

(define (load-global-scores)
  (set! *showing-global?* #t)
  (load-default-screen)

  (add-centered-font default-font24 "GLOBAL SCORES" 275. 24.)

  (set! *loading-obj*
        (add-centered-font default-font24 "loading..." 240. 20.))
  
  (overlay-add-fancy-button "LOCAL"
                            (make-vec2d .09 .855)
                            (lambda (this)
                              (load-local-scores)))
  
  (fetch-global-scores))

(define *showing-global?* #f)
(define *have-scores?* #f)

(define (on-scores-loaded scores)
  (if *loading-obj*
      (overlay-list-remove *loading-obj*))
  
  (if (eq? (current-screen) scores-screen)
      (if scores
          (if (pair? scores)
              (begin
                (update-global-score
                 (persistent-score-score (car scores)))
                (if (and *showing-global?*
                         (not *have-scores?*))
                    (begin
                      (show-scores scores (make-vec4d .3 .3 1. 1.))
                      (set! *have-scores?* #t)))))
          (if *showing-global?*
              (show-feint-enable)))))

(define (show-feint-enable)
  (add-centered-font default-font24 "FEINT IS NOT ENABLED!" 200. 18.)
  (overlay-add-fancy-button "FEINT"
                      (make-vec2d (/ (- 1. *button-width*) 2.) .60)
                      (lambda (this)
                        (feint-open-dashboard))))

(define (load-local-scores)
  (set! *showing-global?* #f)
  (set! *have-scores?* #f)
  
  (load-default-screen)

  (add-centered-font default-font24 "LOCAL SCORES" 275. 24.)
  
  (overlay-add-fancy-button "GLOBAL"
                            (make-vec2d .09 .855)
                            (lambda (this)
                              (load-global-scores)))
  
  (show-scores (get-high-scores)
               (make-vec4d 1. .3 .3 1.)))

(define-screen scores-screen
  init: (lambda ()
          (set! logo-texture (image-opengl-load "logo.png")))
  setup: (lambda ()
           (set! *showing-global?* #f)
           (set! *global-score-obj*
                 (make-2d-object
                  font-perspective
                  font: (make-2d-font default-font24 "--" 20.)
                  position: (to-font-space .6 .34)))
           (load-local-scores)
           (fetch-global-scores))
  run: overlay-update
  render: overlay-render)


