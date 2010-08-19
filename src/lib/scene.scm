;;;; "scene"
;;; Functionality for organizing data in a scene in a heirarchal
;;; order and rendering it

(declare (block)
         (standard-bindings)
         (extended-bindings))

;; `implement-generic-field` uses some cruddy glue to pull together
;; our scene object types into something which looks like a type
;; system with inheritance. Future versions will use a real object
;; system with generic functions such as Meroon.

(define-macro (implement-generic-field name)
  `(begin
     (define (,(symbol-append "generic-object-" name) obj)
       (cond
        ((mesh-object? obj) (,(symbol-append "mesh-object-" name) obj))
        ((2d-object? obj) (,(symbol-append "2d-object-" name) obj))
        ((scene-object? obj) (,(symbol-append "scene-object-" name) obj))
        (else (error "Unsupported scene object type: " obj))))
     (define (,(symbol-append "generic-object-" name "-set!") obj val)
       (cond
        ((mesh-object? obj)
         (,(symbol-append "mesh-object-" name "-set!") obj val))
        ((2d-object? obj)
         (,(symbol-append "2d-object-" name "-set!") obj val))
        ((scene-object? obj)
         (,(symbol-append "scene-object-" name "-set!") obj val))
        (else (error "Unsupported scene object type: " obj))))))

;; various scene object types

(define-type scene-object
  id: 21E2B5C7-B69C-4FFD-899C-00484031261A
  constructor: really-make-scene-object
  perspective
  render-proc
  update-proc
  data
  mark)

(define (make-scene-object pers render-proc update-proc #!optional data)
  (really-make-scene-object pers
                            render-proc
                            update-proc
                            data
                            #f))

(define-type mesh-object
  id: 54553028-EBFD-463B-8E91-D55DE37B4FBF
  constructor: really-make-mesh-object
  perspective
  mesh
  color
  position
  rotation
  scale
  velocity
  acceleration
  render-proc
  update-proc
  data
  nuke?
  last-update
  mark)

(define (make-mesh-object pers
                          #!key
                          mesh
                          color position rotation scale
                          velocity acceleration
                          render update
                          data
                          nuke)
  (really-make-mesh-object pers
                           mesh
                           color position rotation scale
                           velocity acceleration
                           (or render mesh-object-render)
                           update
                           data
                           nuke
                           #f
                           #f))

(define (copy-mesh-object obj)
  (really-make-mesh-object
   (mesh-object-perspective obj)
   (mesh-object-mesh obj)
   (and (mesh-object-color obj)
        (vec4d-copy (mesh-object-color obj)))
   (and (mesh-object-position obj)
        (vec3d-copy (mesh-object-position obj)))
   (and (mesh-object-rotation obj)
        (vec4d-copy (mesh-object-rotation obj)))
   (and (mesh-object-scale obj)
        (vec3d-copy (mesh-object-scale obj)))
   (and (mesh-object-velocity obj)
        (vec3d-copy (mesh-object-velocity obj)))
   (and (mesh-object-acceleration obj)
        (vec3d-copy (mesh-object-acceleration obj)))
   (mesh-object-render-proc obj)
   (mesh-object-update-proc obj)
   (mesh-object-data obj)
   #f
   (mesh-object-last-update obj)
   (mesh-object-mark obj)))

(define-type 2d-font
  id: 3A93EEA4-8616-4123-B775-5DA9826419A5
  constructor: really-make-2d-font
  font
  text
  size)

(define (make-2d-font font text #!optional size)
  (really-make-2d-font font
                       text
                       (or size
                           (ftgl-get-font-face-size font))))

(define-type 2d-object
  id: EB4CFD52-6E14-4C9A-AF49-D8B70334B653
  constructor: really-make-2d-object
  perspective
  color
  position
  local-position
  rotation
  scale
  texture
  font
  center
  render-proc
  update-proc
  mark)

(define (make-2d-object pers
                        #!key
                        color position local-position rotation
                        scale texture font center
                        render update)
  (really-make-2d-object pers
                         color
                         position
                         local-position
                         rotation
                         scale
                         texture
                         font
                         center
                         (or render 2d-object-render)
                         update
                         #f))

(define (mesh-object-render obj)
  (glLoadIdentity)
  
  (let ((mesh (mesh-object-mesh obj))
        (color (mesh-object-color obj))
        (pos (mesh-object-position obj))
        (rot (mesh-object-rotation obj))
        (vel (mesh-object-velocity obj))
        (scale (mesh-object-scale obj))
        (nuke? (mesh-object-nuke? obj)))

    (if color
        (begin
          (with-alloc (color-array (vector->float-array
                                    (vector
                                     (vec4d-x color)
                                     (vec4d-y color)
                                     (vec4d-z color)
                                     (vec4d-w color))))
                      (glMaterialfv GL_FRONT_AND_BACK
                                    GL_DIFFUSE
                                    color-array))
          (glColor4f (vec4d-x color)
                     (vec4d-y color)
                     (vec4d-z color)
                     (vec4d-w color))))
    
    (glVertexPointer 3 GL_FLOAT 0 (obj-vertices mesh))
    (glNormalPointer GL_FLOAT 0 (->void-array (obj-normals mesh)))
    (glEnableClientState GL_NORMAL_ARRAY)

    (if pos
        (glTranslatef (vec3d-x pos) (vec3d-y pos) (vec3d-z pos)))

    (if rot
        (glRotatef (vec4d-w rot)
                   (vec4d-x rot)
                   (vec4d-y rot)
                   (vec4d-z rot)))

    
    (if scale
        (glScalef (vec3d-x scale) (vec3d-y scale) (vec3d-z scale)))

    (if nuke?
        (glScalef .5 .5 .5))
    
    (glEnable GL_BLEND)
    (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)

    (for-each
     (lambda (chunk)
       (if (obj-chunk-mat chunk)
           (let* ((mat (obj-chunk-mat chunk))
                  (color (if color
                             (vec4d-component-mul
                              color
                              (material-diffuse mat))
                             (material-diffuse mat))))

             (if (< (vec4d-w color) 1.)
                 (glDisable GL_DEPTH_TEST)
                 (glEnable GL_DEPTH_TEST))

             (if nuke?
                 (begin
                   (glColor4f 0. 1. 0. .5)
                   (with-alloc (color-array (vector->float-array
                                             (vector 0. 1. 0. .5)))
                               (glMaterialfv GL_FRONT_AND_BACK
                                             GL_DIFFUSE
                                             color-array)))
                 (begin
                   (with-alloc (color-array (vector->float-array
                                             (vector
                                              (vec4d-x color)
                                              (vec4d-y color)
                                              (vec4d-z color)
                                              (vec4d-w color))))
                               (glMaterialfv GL_FRONT_AND_BACK
                                             GL_DIFFUSE
                                             color-array))
                   (glColor4f (+ (vec4d-x color) .1)
                              (+ (vec4d-y color) .1)
                              (+ (vec4d-z color) .1)
                              (vec4d-w color))))))

       (if (not (null? (obj-chunk-indices chunk)))
           (glDrawElements GL_TRIANGLES
                           (obj-chunk-num-indices chunk)
                           GL_UNSIGNED_SHORT
                           (->void-array (obj-chunk-indices chunk)))))
     (obj-chunks mesh))

    (glEnable GL_DEPTH_TEST)
    (glDisable GL_BLEND)
    (glDisableClientState GL_NORMAL_ARRAY)))

(define (textured-object? obj)
  (let ((obj (if (scene-object? obj)
                 (scene-object-data obj)
                 obj)))
    (and (2d-object? obj)
         (or (2d-object-texture obj)
             (2d-object-font obj)))))

(define *2d-object-stack* 0)

(define (2d-object-prerender #!optional textured?)
  (if (eq? *2d-object-stack* 0)
      (begin
        (if textured?
            (glEnable GL_TEXTURE_2D))
        (glDisable GL_LIGHTING)
        (glDisable GL_DEPTH_TEST)))
  
  (set! *2d-object-stack*
        (+ *2d-object-stack* 1)))

(define (2d-object-postrender #!optional textured?)
  (set! *2d-object-stack*
        (- *2d-object-stack* 1))

  (if (eq? *2d-object-stack* 0)
      (begin
        (if textured?
            (glDisable GL_TEXTURE_2D))
        (glEnable GL_LIGHTING)
        (glEnable GL_DEPTH_TEST))))

(define (2d-object-render obj)
  (glLoadIdentity)
  (2d-object-prerender (textured-object? obj))
  
  (let ((color (2d-object-color obj))
        (pos (2d-object-position obj))
        (local-pos (2d-object-local-position obj))
        (rot (2d-object-rotation obj))
        (scale (2d-object-scale obj))
        (texture (2d-object-texture obj))
        (font (2d-object-font obj))
        (center (2d-object-center obj)))

    (if pos
        (glTranslatef (vec3d-x pos) (vec3d-y pos) (vec3d-z pos)))

    (if rot
        (glRotatef (vec4d-w rot)
                   (vec4d-x rot)
                   (vec4d-y rot)
                   (vec4d-z rot)))

    (if local-pos
        (glTranslatef (vec3d-x local-pos) (vec3d-y local-pos) (vec3d-z local-pos)))
    
    (if scale
        (glScalef (vec2d-x scale) (vec2d-y scale) 1.))

    (if center
        (if (vec3d? center)
            (glTranslatef (- (vec3d-x center))
                          (- (vec3d-y center))
                          (- (vec3d-z center)))
            (glTranslatef -.5 -.5 0.)))
    
    ;; Todo:
    ;; Implement centering (LEFT, CENTER, RIGHT)
    ;; (glTranslatef -x/2 -y/2 0.)

    (if (or (and texture
                 (img-alpha? texture))
            font
            (and color
                 (< (vec4d-w color) 1.)))
        (begin
          (glEnable GL_BLEND)
          (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)))
    
    (if color
        (glColor4f (vec4d-x color)
                   (vec4d-y color)
                   (vec4d-z color)
                   (vec4d-w color))
        (glColor4f 1. 1. 1. 1.))

    (if font
        (begin
          (glDisable GL_CULL_FACE)

          (ftgl-prepare-fonts)
          (let ((scale (exact->inexact
                        (/ (2d-font-size font)
                           (ftgl-get-font-face-size
                            (2d-font-font font))))))
            (glScalef scale scale 1.))
          (ftgl-render-font (2d-font-font font)
                            (2d-font-text font))

          (glEnable GL_CULL_FACE)
          (image-reset-texture))
        (if texture
            (image-render texture)
            (image-render-base)))

    (2d-object-postrender (textured-object? obj))))

(implement-generic-field perspective)
(implement-generic-field color)
(implement-generic-field scale)
(implement-generic-field position)
(implement-generic-field local-position)
(implement-generic-field rotation)
(implement-generic-field mark)
(implement-generic-field render-proc)
(implement-generic-field update-proc)

(define (render-generic-object obj)
  ((generic-object-render-proc obj) obj))

(define (update-generic-object obj)
  (let ((update (generic-object-update-proc obj)))
    (if update
        (update obj)
        #t)))

(define scene-list '())

(define (scene-list-clear!)
  (set! scene-list '()))

(define (scene-list-add obj #!key important unimportant)
  (cond
   (important
    (set! scene-list (append scene-list (list obj))))
   (unimportant
    (set! scene-list (cons obj scene-list)))
   (else
    ;; keep the order according to the projection matrix to minimize
    ;; matrix changes
    (receive (prefix tail)
        (break (lambda (el)
                 (eq? (generic-object-perspective el)
                      (generic-object-perspective obj)))
               scene-list)
      (receive (same tail2)
          (span (lambda (el)
                  (eq? (generic-object-perspective el)
                       (generic-object-perspective obj)))
                tail)
        (set! scene-list (append prefix same (cons obj tail2))))))))

(define (scene-list-remove obj)
  (generic-object-mark-set! obj #t))

(define (scene-list-purely-updates #!optional update-fn skip-local-update local-list)
  (for-each (lambda (obj)
              (if (not skip-local-update)
                  (update-generic-object obj))
              (if update-fn (update-fn obj)))
            (or local-list scene-list)))

(define (scene-list-purely-removes #!optional local-list)
  (let ((lst (reverse
              (fold (lambda (obj acc)
                      (if (generic-object-mark obj)
                          acc
                          (cons obj acc)))
                    '()
                    (or local-list scene-list)))))
    (if local-list
        lst
        (set! scene-list lst))))

(define *pause* #f)

(define (scene-list-pause)
  (set! *pause* #t))

(define (scene-list-unpause)
  (set! *pause* #f))

(define (scene-list-paused?)
  *pause*)

(define (scene-list-update #!optional update-fn skip-local-update local-list)
  (if (not (scene-list-paused?))
      (begin
        ;; run the update procedures for each scene object
        (scene-list-purely-updates update-fn skip-local-update local-list)
  
        ;; remove all the objects marked for removal
        (scene-list-purely-removes local-list))
      (if local-list local-list scene-list)))

(define (scene-list-render #!optional render-proc local-list)
  (let loop ((last-pers #f)
             (tail (or local-list scene-list)))
    (if (not (null? tail))
        (let* ((head (car tail))
               (pers (generic-object-perspective head)))
          (if (or (not last-pers)
                  (not (eq? last-pers pers)))
              (load-perspective pers))
          (if render-proc
              (render-proc head)
              (render-generic-object head))
          (loop pers (cdr tail))))))

;; (define (scene-list-render #!optional render-proc local-list)
;;   (2d-object-prerender)
;;   (%scene-list-render textured-object?
;;                       render-proc
;;                       local-list)
;;   (2d-object-postrender)
;;   (%scene-list-render (lambda (obj)
;;                         (not (textured-object? obj)))
;;                       render-proc
;;                       local-list))
