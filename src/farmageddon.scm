
(declare (block)
         (standard-bindings)
         (extended-bindings))

;; libraries

(include "ffi/ffi#.scm")
(include "lib/obj-loader.scm")
(include "lib/macros.scm")
(include "lib/srfi/srfi-2.scm")
(include "lib/srfi/srfi-13.scm")
(include "lib/events#.scm")
(include "lib/vectors.scm")
(include "lib/scene.scm")
(include "lib/physics.scm")
(include "lib/standard-meshes.scm")
(include "lib/tween.scm")
(include "lib/texture.scm")
(include "lib/matrix-util.scm")
(include "lib/quaternion.scm")
(include "button.scm")

;; install all the screens of the game

(include "screens.scm")

;; various global state

(define current-perspective (make-parameter #f))

(define (load-perspective pers)
  (current-perspective pers)
  (glMatrixMode GL_PROJECTION)
  (glLoadMatrixf (perspective-matrix pers))
  (glMatrixMode GL_MODELVIEW)
  (glLoadIdentity))

;; engine

(define (default-opengl)
  (glEnableClientState GL_VERTEX_ARRAY)
  (glEnableClientState GL_TEXTURE_COORD_ARRAY))

(define (init)
  (random-source-randomize! default-random-source)
  (default-opengl)
  (set-screen! load-screen))

(define *time-start* (real-time))
(define *frames* 0)

(define (render)
  ;; (if (not *time-start*)
  ;;     (begin
  ;;       (set! *time-start* (real-time))
  ;;       (profile-start!)))

  (current-screen-run)
  (glClearColor 0. .0 .0 1.)
  (glClear (bitwise-ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
  (current-screen-render)

  ;; (##gc)

  ;; (if (> (- (real-time) *time-start*) 10.)
  ;;     (begin
  ;;       (profile-stop!)
  ;;       (write-profile-report "/tmp/farmageddon/")
  ;;       (exit)))

  #;
  (if (> (- (real-time) *time-start*) 1.)
      (begin
        (update-counter *frames*)
        (set! *frames* 0)
        (set! *time-start* (real-time)))
      (set! *frames* (+ *frames* 1)))

  (thread-yield!))
