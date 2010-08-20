;;;; levels
;;; Implements a system for different levels of the game, actual
;;; levels are implemented at the bottom.

(declare (block)
         (standard-bindings)
         (extended-bindings))

(define current-max-life (make-parameter #f))
(define current-background-texture (make-parameter #f))
(define current-animal-frequency (make-parameter #f))
(define current-available-meshes (make-parameter #f))

(define (scene-init)
  (current-max-life 14)
  (current-background-texture level-bg)
  (current-animal-frequency .5)
  (current-available-meshes #f)

  (glLoadIdentity)

  ;; Set up the lighting. We want it pretty lit up, but some shading
  ;; to show form. Provide backlighting for ambience, and also cast
  ;; light forward to really brighten it up. Both directional with
  ;; lights with white ambience.
  (glEnable GL_LIGHTING)
  (glEnable GL_LIGHT0)

  (with-alloc (color (vector->float-array (vector 1. 1. 1. 1.)))
              (glLightfv GL_LIGHT0 GL_AMBIENT color))
  
  (with-alloc (color (vector->float-array (vector 0. 0. -1. 0.)))
              (glLightfv GL_LIGHT0 GL_POSITION color))
  
  (with-alloc (color (vector->float-array (vector 1. 1. 1. 1.)))
              (glLightfv GL_LIGHT0 GL_DIFFUSE color))
  
  (with-alloc (color (vector->float-array (vector 1. 1. 1. 1.)))
              (glLightfv GL_LIGHT1 GL_AMBIENT color))
  
  (with-alloc (color (vector->float-array (vector 0. 0. 1. 0.)))
              (glLightfv GL_LIGHT1 GL_POSITION color))
  
  (with-alloc (color (vector->float-array (vector 1. 1. 1. 1.)))
              (glLightfv GL_LIGHT1 GL_DIFFUSE color))
  
  (with-alloc (color (vector->float-array (vector .3 .3 .3 1.)))
              (glLightModelfv GL_LIGHT_MODEL_AMBIENT color))

  (glDisable GL_FOG))
