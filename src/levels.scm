;;;; levels
;;; Implements a system for different levels of the game, actual
;;; levels are implemented at the bottom.

(define (atmosphere-reset)
  (glLoadIdentity)

  ;; Set up the lighting. We want it pretty lit up, but some shading
  ;; to show form. Provide backlighting for ambience, and also cast
  ;; light forward to really brighten it up. Both directional with
  ;; lights with white ambience.
  (glEnable GL_LIGHTING)
  (glEnable GL_LIGHT0)
  (glLightfv GL_LIGHT0 GL_AMBIENT (vector->float-array (vector 1. 1. 1. 1.)))
  (glLightfv GL_LIGHT0 GL_POSITION (vector->float-array (vector 0. 0. -1. 0.)))
  (glLightfv GL_LIGHT0 GL_DIFFUSE (vector->float-array (vector 1. 1. 1. 1.)))
  (glLightfv GL_LIGHT1 GL_AMBIENT (vector->float-array (vector 1. 1. 1. 1.)))
  (glLightfv GL_LIGHT1 GL_POSITION (vector->float-array (vector 0. 0. 1. 0.)))
  (glLightfv GL_LIGHT1 GL_DIFFUSE (vector->float-array (vector 1. 1. 1. 1.)))
  (glLightModelfv GL_LIGHT_MODEL_AMBIENT (vector->float-array (vector .3 .3 .3 1.)))

  (glDisable GL_FOG)

  ;; This value is rather arbitrary; it just depends on how heavy we
  ;; set everything in the scene. We are using a gravity with an
  ;; acceleration of 11 m/s as a reference point.
  (set! GRAVITY (make-vec3d 0. -11. 0.)))

(define-macro (implement-level-type . fields) 
  `(begin
     (define-type level ,@fields)

     ,@(map (lambda (field)
              `(define (,(symbol-append "current-" field))
                 (,(symbol-append "level-" field) CURRENT-LEVEL)))
            fields)))

(implement-level-type
  level-name
  level-goal
  background-texture
  animal-frequency
  available-meshes
  atmosphere-creator)

(define CURRENT-LEVEL #f)

(define (set-level! level)
  (set! CURRENT-LEVEL level)
  (score-reset)
  (atmosphere-reset)
  (and-let* ((atmosphere (current-atmosphere-creator)))
    (atmosphere)))

;; levels

(define (basic-level)
  (make-level "basic"
              5
              sky-texture
              2.5
              #f
              #f))

(define (fog-level)
  (make-level
   "fog"
   7
   #f
   2.
   #f
   (lambda ()
     (glLoadIdentity)
     (glFogx GL_FOG_MODE GL_LINEAR)
     (glFogfv GL_FOG_COLOR (vector->float-array (vector 0. 0. 0. 1.)))
     ;;(glFogf GL_FOG_DENSITY 1.)
     (glFogf GL_FOG_START 1.)
     (glFogf GL_FOG_END 30.)
     (glEnable GL_FOG))))

(define (chicken-frenzy-level)
  (make-level "chicken-frenzy"
              1000
              gradient-texture
              .3
              (list chicken-mesh)
              #f))
