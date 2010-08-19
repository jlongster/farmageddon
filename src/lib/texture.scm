
(declare (block)
         (standard-bindings)
         (extended-bindings))

(include "../ffi/ffi#.scm")

(define-type img
  id
  alpha?)

(define (alloc-opengl-image)
  (with-alloc (img (make-unsigned-int-array 1))
    (glGenTextures 1 img)
    (unsigned-int-array-ref img 0)))

(define (image-opengl-load name)
  (let ((ext (path-extension name))
        (path (resource name)))
    (if (equal? ext ".pvr")
        (let ((pvr (pvr-load path)))
          (make-img
           (image-pvr-upload pvr)
           (pvr-alpha pvr)))
        (let* ((image (CGImageRef-load name)))
          (make-img
           (image-opengl-upload (CGImageRef-data image)
                                (CGImageRef-width image)
                                (CGImageRef-height image))
           #t)))))

(define (image-opengl-upload data width height)
  (let ((tex (alloc-opengl-image)))
    (glBindTexture GL_TEXTURE_2D tex)
    (glTexImage2D GL_TEXTURE_2D
                  0
                  GL_RGBA
                  width
                  height
                  0
                  GL_RGBA
                  GL_UNSIGNED_BYTE
                  (->void-array data))
    (image-apply-settings)
    tex))

(define (image-pvr-upload pvr)
  (let ((tex (alloc-opengl-image)))
    (glBindTexture GL_TEXTURE_2D tex)
    (glCompressedTexImage2D GL_TEXTURE_2D
                            0
                            (pvr-format pvr)
                            (pvr-width pvr)
                            (pvr-height pvr)
                            0
                            (pvr-data-size pvr)
                            (pvr-data pvr))
    (image-apply-settings)
    tex))

(define (image-apply-settings)
  (glTexEnvi GL_TEXTURE_ENV GL_TEXTURE_ENV_MODE GL_MODULATE)
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
  (glBindTexture GL_TEXTURE_2D 0))

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

(define *last-texture* #f)

(define (image-reset-texture)
  (set! *last-texture* #f))

(define (image-render img)
  (glBindTexture GL_TEXTURE_2D (img-id img))
  (glEnable GL_TEXTURE_2D)
  (glDisable GL_LIGHTING)
  (glDisable GL_DEPTH_TEST)
  
  (glVertexPointer 2 GL_FLOAT 0 square-pos)
  (glTexCoordPointer 2 GL_FLOAT 0 square-texcoords)
  
  (glDrawArrays GL_TRIANGLE_STRIP 0 4)
  
  (glEnable GL_LIGHTING)
  (glEnable GL_DEPTH_TEST)
  (glDisable GL_TEXTURE_2D))

(define (image-render-base)
  (glDisable GL_DEPTH_TEST)
  (glDisable GL_LIGHTING)

  (glVertexPointer 2 GL_FLOAT 0 square-pos)
  (glDrawArrays GL_TRIANGLE_STRIP 0 4)

  (glEnable GL_DEPTH_TEST)
  (glEnable GL_LIGHTING))
