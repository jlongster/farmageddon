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
  mark)

(define (make-scene-object pers render-proc update-proc)
  (really-make-scene-object pers
                            render-proc
                            update-proc
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
  last-update
  voice-source
  thud-source
  mark)

(define (make-mesh-object pers
                          #!key
                          mesh
                          color position rotation scale
                          velocity acceleration
                          render update
                          data)
  (really-make-mesh-object pers
                           mesh
                           color position rotation scale
                           velocity acceleration
                           (or render mesh-object-render)
                           update
                           data
                           #f
                           #f
                           #f
                           #f))

(define-type 2d-object
  id: EB4CFD52-6E14-4C9A-AF49-D8B70334B653
  constructor: really-make-2d-object
  perspective
  color
  position
  rotation
  scale
  texture
  render-proc
  update-proc
  mark)

(define (make-2d-object pers
                        #!key
                        color position rotation scale texture
                        render update)
  (really-make-2d-object pers
                         color
                         position
                         rotation
                         scale
                         texture
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
        (scale (mesh-object-scale obj)))

    (glVertexPointer 3 GL_FLOAT 0 (obj-vertices mesh))
    (glEnableClientState GL_VERTEX_ARRAY)
    (glNormalPointer GL_FLOAT 0 (->void-array (obj-normals mesh)))
    (glEnableClientState GL_NORMAL_ARRAY)

    (if color
        (begin
          (glMaterialfv GL_FRONT_AND_BACK
                        GL_DIFFUSE
                        (vector->float-array
                         (vector
                          (vec3d-x color)
                          (vec3d-y color)
                          (vec3d-z color)
                          1.)))
          (glColor4f (vec3d-x color)
                     (vec3d-y color)
                     (vec3d-z color)
                     1.)))

    (if pos
        (glTranslatef (vec3d-x pos) (vec3d-y pos) (vec3d-z pos)))

    (if rot
        (glRotatef (vec4d-w rot)
                   (vec4d-x rot)
                   (vec4d-y rot)
                   (vec4d-z rot)))

    (if scale
        (glScalef (vec3d-x scale) (vec3d-y scale) (vec3d-z scale)))

    (for-each (lambda (chunk)
                (if (obj-chunk-mat chunk)
                    (let* ((mat (obj-chunk-mat chunk))
                           (d (material-diffuse mat)))
                      (with-alloc (color-array
                                   (vector->float-array
                                    (vector
                                     (vec3d-x d) (vec3d-y d) (vec3d-z d) 1.)))
                        (glMaterialfv GL_FRONT_AND_BACK
                                      GL_DIFFUSE
                                      color-array))
                      (glColor4f (vec3d-x d)
                                 (vec3d-y d)
                                 (vec3d-z d)
                                 1.)))
                (if (not (null? (obj-chunk-indices chunk)))
                    (glDrawElements GL_TRIANGLES
                                    (obj-chunk-num-indices chunk)
                                    GL_UNSIGNED_SHORT
                                    (->void-array (obj-chunk-indices chunk)))))
              (obj-chunks mesh))

    (glDisableClientState GL_NORMAL_ARRAY)))

(define (2d-object-render obj)
  (glLoadIdentity)
  
  (let ((color (2d-object-color obj))
        (pos (2d-object-position obj))
        (rot (2d-object-rotation obj))
        (scale (2d-object-scale obj))
        (texture (2d-object-texture obj)))
    (if pos
        (glTranslatef (vec3d-x pos) (vec3d-y pos) (vec3d-z pos)))

    (if rot
        (glRotatef (vec4d-w rot)
                   (vec4d-x rot)
                   (vec4d-y rot)
                   (vec4d-z rot)))
    
    (if scale
        (glScalef (vec2d-x scale) (vec2d-y scale) 1.))

    (if color
        (glColor4f (vec4d-x color)
                   (vec4d-y color)
                   (vec4d-z color)
                   (vec4d-w color)))

    (glEnable GL_BLEND)
    (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)

    (if texture
        (image-render texture)
        (image-render-base))

    (glColor4f 1. 1. 1. 1.)
    (glDisable GL_BLEND)))

(implement-generic-field perspective)
(implement-generic-field color)
(implement-generic-field scale)
(implement-generic-field position)
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

(define (scene-list-add obj)
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
      (set! scene-list (append prefix same (cons obj tail2))))))

(define (scene-list-update #!optional update-fn)
  ;; run the update procedures for each scene object
  (for-each (lambda (obj)
              (if (update-generic-object obj)
                  ((or update-fn values) obj)
                  (generic-object-mark-set! obj #t)))
            scene-list)

  ;; remove all the objects marked for removal
  (set! scene-list
        (reverse
         (fold (lambda (obj acc)
                 (if (generic-object-mark obj)
                     acc
                     (cons obj acc)))
               '()
               scene-list))))

(define (scene-list-render #!optional render-proc)
  (let loop ((last-pers #f)
             (tail scene-list))
    (if (not (null? tail))
        (let* ((head (car tail))
               (pers (generic-object-perspective head)))
          (if (not last-pers)
              (load-perspective pers))
          (if render-proc
              (render-proc head)
              (render-generic-object head))
          (loop pers (cdr tail))))))
