;;;; levels
;;; Implements a system for different levels of the game, actual
;;; levels are implemented at the bottom.

(declare (block)
         (standard-bindings)
         (extended-bindings))

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
     (define-type level
       constructor: really-make-level
       ,@fields)

     ,@(map (lambda (field)
              `(define (,(symbol-append "current-" field))
                 (,(symbol-append "level-" field) CURRENT-LEVEL)))
            fields)))

(implement-level-type
  level-name
  level-goal
  goal-type
  max-life
  background-texture
  animal-frequency
  available-meshes
  atmosphere-creator)

(define (make-level #!key
                    name
                    goal
                    type
                    life
                    background
                    frequency
                    meshes
                    atmosphere)
  (really-make-level name
                     goal
                     type
                     life
                     background
                     frequency
                     meshes
                     atmosphere))

(define CURRENT-LEVEL #f)

(define (set-level! level)
  (set! CURRENT-LEVEL level)
  (atmosphere-reset)
  (and-let* ((atmosphere (current-atmosphere-creator)))
    (atmosphere)))

(define (current-level)
  CURRENT-LEVEL)

(define (first-level)
  (set-level! (car (all-levels))))

(define (next-level)
  (let ((tail (find-tail
               (lambda (el)
                 (equal? (current-level-name)
                         (level-level-name el)))
               (all-levels))))
    (if (null? (cdr tail))
        #f
        (begin
          (set-level! (cadr tail))
          #t))))

(define (all-levels)
  (list (level1)
        (level2)
        (chicken-frenzy-level)))
 
;; levels

(define (level1)
  (make-level
   name: 1
   goal: 5
   type: 'goal
   life: 10
   background: level-bg1
   frequency: 1.
   meshes: (list chicken-mesh duck-mesh)))

(define (level2)
  (make-level
   name: 2
   goal: 5
   type: 'goal
   life: 10
   background: level-bg2
   frequency: 1.
   meshes: (list chicken-mesh duck-mesh sheep-mesh)))

(define (level3)
  (make-level
   name: 3
   goal: 8
   type: 'goal
   life: 10
   background: level-bg2
   frequency: 1.
   meshes: (list chicken-mesh
                           duck-mesh
                           sheep-mesh
                           cow-mesh)))

(define (chicken-frenzy-level)
  (make-level
   name: "CHICKEN FRENZY"
   goal: 10
   type: 'timer
   life: 10
   background: level-bg3
   frequency: .8
   meshes: (list chicken-mesh)))


;; (define (fog-level)
;;   (make-level
;;    "fog"
;;    7
;;    'timer
;;    #f
;;    2.
;;    #f
;;    (lambda ()
;;      (glLoadIdentity)
;;      (glFogx GL_FOG_MODE GL_LINEAR)
;;      (glFogfv GL_FOG_COLOR (vector->float-array (vector 0. 0. 0. 1.)))
;;      ;;(glFogf GL_FOG_DENSITY 1.)
;;      (glFogf GL_FOG_START 1.)
;;      (glFogf GL_FOG_END 30.)
;;      (glEnable GL_FOG))))
