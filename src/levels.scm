
(define (make-level1)
  (basic-level))

(define (next-level-points! n)
  (lambda ()
    (if (>= (get-raw-score) 10)
        (next-level!))))

(define (basic-level)
  (make-level "basic"
              (lambda ()
                (set! LVL_FREQUENCY 2.)
                (set! background-texture gradient-texture))
              values
              values
              (lambda ()
                (fog-level))))

(define (fog-level)
  (define (atmosphere)
    (glLoadIdentity)
    (default-atmosphere)
    (glFogx GL_FOG_MODE GL_LINEAR)
    (glFogfv GL_FOG_COLOR (vector->float-array (vector 0. 0. 0. 1.)))
    ;;(glFogf GL_FOG_DENSITY 1.)
    (glFogf GL_FOG_START 1.)
    (glFogf GL_FOG_END 30.)
    (glEnable GL_FOG)
    (set! background-texture #f))

  (make-level "fog"
              (lambda ()
                (set! LVL_FREQUENCY 2.)
                (set! LVL_ATMOSPHERE atmosphere))
              (next-level-points! 7)
              values
              (lambda ()
                (chicken-frenzy-level))))

(define (chicken-frenzy-level)
  (make-level "chicken-frenzy"
              (lambda ()
                (set! LVL_FREQUENCY .3)
                (set! LVL_GET_MESH
                      (lambda ()
                        chicken-mesh))
                (set! background-texture gradient-texture))
              values
              values
              values))
