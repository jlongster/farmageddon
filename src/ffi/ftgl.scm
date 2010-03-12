;;;; "ftgl"
;;; Interfaces with the ftgl library for rendering fonts in OpenGL.

(c-declare "#include \"FTGL/ftgles.h\"")
(include "ftgl#.scm")

(define ftgl-prepare-fonts
  (c-lambda () void "ftglInitImmediateModeGL"))
    
(define ftgl-create-texture-font
  (c-lambda (char-string) FTGLFont* "ftglCreateTextureFont"))

(define ftgl-create-buffer-font
  (c-lambda (char-string) FTGLFont* "ftglCreateBufferFont"))

(define (ftgl-set-font-face-size font size . dpi)
  ((c-lambda (FTGLFont* unsigned-int unsigned-int) unsigned-int
             "ftglSetFontFaceSize")
   font size (if (null? dpi) 0 (car dpi))))

(define ftgl-get-font-face-size
  (c-lambda (FTGLFont*) unsigned-int
            "ftglGetFontFaceSize"))

(define ftgl-get-font-advance
  (c-lambda (FTGLFont* char-string) float "ftglGetFontAdvance"))

(define (ftgl-render-font font str . mode)
  ((c-lambda (FTGLFont* char-string unsigned-int) void
             "ftglRenderFont")
   font str (if (null? mode) FTGL_RENDER_ALL (car mode))))

(define ftgl-get-font-error
  (c-lambda (FTGLFont*) FT_Error "ftglGetFontError"))
