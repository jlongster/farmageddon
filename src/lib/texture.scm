
(declare (block)
         (standard-bindings)
         (extended-bindings))

(include "../ffi/ffi#.scm")

(define (alloc-opengl-image)
  (with-alloc (img (make-unsigned-int-array 1))
    (glGenTextures 1 img)
    (unsigned-int-array-ref img 0)))

(define (image-opengl-load name)
  (let ((image (CGImageRef-load name)))
    (image-opengl-upload (CGImageRef-data image)
                         (CGImageRef-width image)
                         (CGImageRef-height image))))

(define (image-opengl-upload data width height)
  (let ((tex (alloc-opengl-image)))
    (glBindTexture GL_TEXTURE_2D tex)
    (glTexEnvi GL_TEXTURE_ENV GL_TEXTURE_ENV_MODE GL_MODULATE)
    (glTexImage2D GL_TEXTURE_2D
                  0
                  GL_RGBA
                  width
                  height
                  0
                  GL_RGBA
                  GL_UNSIGNED_BYTE
                  (->void-array data))
    (glTexParameteri GL_TEXTURE_2D
                     GL_TEXTURE_MIN_FILTER
                     GL_LINEAR)
    (glTexParameteri GL_TEXTURE_2D
                     GL_TEXTURE_MAG_FILTER
                     GL_LINEAR)
    (glTexParameteri GL_TEXTURE_2D
                     GL_TEXTURE_WRAP_S
                     GL_CLAMP_TO_EDGE)
    (glTexParameteri GL_TEXTURE_2D
                     GL_TEXTURE_WRAP_T
                     GL_CLAMP_TO_EDGE)    
    (glBindTexture GL_TEXTURE_2D 0)
    tex))

(define square-pos
  (vector->float-array (vector 0. 0.
                               0. 1.
                               1. 0.
                               1. 1.)))

(define square-texcoords
  (->void-array
   (vector->float-array (vector 0. 0.
                             0. 1.
                             1. 0.
                             1. 1.))))

(define (image-render tex)
  (glBindTexture GL_TEXTURE_2D tex)

  (glEnable GL_TEXTURE_2D)
  (glDisable GL_DEPTH_TEST)
  (glDisable GL_LIGHTING)

  (glVertexPointer 2 GL_FLOAT 0 square-pos)
  (glEnableClientState GL_VERTEX_ARRAY)
  
  (glTexCoordPointer 2 GL_FLOAT 0 square-texcoords)
  (glEnableClientState GL_TEXTURE_COORD_ARRAY)
  
  (glDrawArrays GL_TRIANGLE_STRIP 0 4)

  (glDisableClientState GL_TEXTURE_COORD_ARRAY)
  (glEnable GL_DEPTH_TEST)
  (glDisable GL_TEXTURE_2D)
  (glEnable GL_LIGHTING))

(define (image-render-base)
  (glDisable GL_DEPTH_TEST)
  (glDisable GL_LIGHTING)

  (glVertexPointer 2 GL_FLOAT 0 square-pos)
  (glEnableClientState GL_VERTEX_ARRAY)
  (glDrawArrays GL_TRIANGLE_STRIP 0 4)
  
  (glEnable GL_DEPTH_TEST)
  (glEnable GL_LIGHTING))
