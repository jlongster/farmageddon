
(define line (vector->float-array (vector 0. 0. 0.
                                          1. 0. 0.)))

(define (render-grid grid-size)
  (define (draw)
    (glScalef grid-size 0. 0.)
    (glDrawArrays GL_LINES 0 2))

  (define offset (/ grid-size 2.))
  
  (define (render-lines)
    (unfold (lambda (i) (> i grid-size))
            (lambda (i)
              (reset-camera)
              ;;(glRotatef 45. 1. 0. 0.)
              (glTranslatef (- offset) -5. (- (exact->inexact i) offset))
              (draw))
            (lambda (i) (+ i 1))
            0)

    (unfold (lambda (i) (> i grid-size))
            (lambda (i)
              (reset-camera)
              ;;(glRotatef 45. 0. 0. 0.)
              (glTranslatef (- (exact->inexact i) offset) -5. (- offset))
              (glRotatef -90. 0. 1. 0.)
              (draw))
            (lambda (i) (+ i 1))
            0))
  
  (glDisable GL_LIGHTING)
  (glVertexPointer 3 GL_FLOAT 0 line)
  (glColor4f 0. 1. 0. 1.)
  (render-lines)  
  (glEnable GL_LIGHTING))
