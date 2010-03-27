
(declare (block)
         (standard-bindings)
         (extended-bindings))

(include "../ffi/ffi#.scm")

(define (alloc-opengl-image)
  (with-alloc (img (make-unsigned-int-array 1))
    (glGenTextures 1 img)
    (unsigned-int-array-ref img 0)))

(define (image-opengl-load name)
  (let ((ext (path-extension name))
        (path (resource name)))
    (if (equal? ext ".pvr")
        (image-pvr-upload (pvr-load path))
        (let* ((image (CGImageRef-load path)))
          (image-opengl-upload (CGImageRef-data image)
                               (CGImageRef-width image)
                               (CGImageRef-height image))))))

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
                   GL_NEAREST)
  (glTexParameteri GL_TEXTURE_2D
                   GL_TEXTURE_MAG_FILTER
                   GL_NEAREST)
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

(define (image-render tex)
  (if (not (eq? tex *last-texture*))
      (begin
        (glBindTexture GL_TEXTURE_2D tex)
        (set! *last-texture* tex)))
  
  (glVertexPointer 2 GL_FLOAT 0 square-pos)
  (glTexCoordPointer 2 GL_FLOAT 0 square-texcoords)  
  (glDrawArrays GL_TRIANGLE_STRIP 0 4))

(define (image-render-base)
  (glDisable GL_DEPTH_TEST)
  (glDisable GL_LIGHTING)

  (glVertexPointer 2 GL_FLOAT 0 square-pos)
  (glDrawArrays GL_TRIANGLE_STRIP 0 4)
  
  (glEnable GL_DEPTH_TEST)
  (glEnable GL_LIGHTING))
