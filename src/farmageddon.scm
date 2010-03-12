
(declare (block)
         (standard-bindings)
         (extended-bindings))

;; libraries

(include "ffi/ffi#.scm")
(include "lib/macros.scm")
(include "lib/srfi/srfi-1.scm")
(include "lib/srfi/srfi-2.scm")
(include "lib/srfi/sort.scm")
(include "lib/vectors.scm")
(include "lib/events#.scm")
(include "lib/events.scm")
(include "lib/obj-loader.scm")
(include "lib/scene.scm")
(include "lib/physics.scm")
(include "lib/standard-meshes.scm")
(include "lib/tween.scm")
(include "lib/texture.scm")
(include "lib/matrix-util.scm")
(include "lib/quaternion.scm")

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

(define (init)
  (random-source-randomize! default-random-source)
  (set-screen! title-screen))

(define (render)
  (force-output (repl-output-port))
  (current-screen-run)

  (glClearColor 0. .0 .0 1.)
  (glClear (bitwise-ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
  (current-screen-render)
  
  (##gc))
