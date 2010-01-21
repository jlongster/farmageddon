;;;; "scene"
;;; Functionality for organizing data in a scene in a heirarchal
;;; order and rendering it

(declare (block)
         (standard-bindings)
         (extended-bindings))

(define-type scene-object
  id: 21E2B5C7-B69C-4FFD-899C-00484031261A
  projection-matrix
  render-proc
  update-proc)

(define-type mesh-object
  id: 54553028-EBFD-463B-8E91-D55DE37B4FBF
  constructor: really-make-mesh-object
  projection-matrix
  mesh
  color
  position
  rotation
  scale
  velocity
  acceleration
  update-proc
  %%last-update
  data
  voice-source
  thud-source)

(define (make-mesh-object proj-matrix mesh color pos #!optional rot scale vel accel update data)
  (really-make-mesh-object proj-matrix
                           mesh color pos rot scale vel accel
                           (or update values)
                           #f
                           data
                           #f
                           #f))

(define-type 2d-object
  id: EB4CFD52-6E14-4C9A-AF49-D8B70334B653
  constructor: really-make-2d-object
  projection-matrix
  update-proc
  color
  position
  rotation
  scale
  texture)

(define (make-2d-object proj-matrix
                        #!key
                        update
                        color position rotation scale texture)
  (really-make-2d-object proj-matrix
                         (or update values)
                         color
                         position
                         rotation
                         scale
                         texture))

(define (mesh-object-render obj)
  (glLoadMatrix (generic-object-projection-matrix obj))
  
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
  (glLoadMatrix (generic-object-projection-matrix obj))
  
  (let ((color (2d-object-color obj))
        (pos (2d-object-position obj))
        (rot (2d-object-rotation obj))
        (scale (2d-object-scale obj))
        (texture (2d-object-texture obj)))
    (if pos
        (glTranslatef (vec2d-x pos) (vec2d-y pos) 0.))

    (if rot
        (glRotatef rot 0. 0. 1.))

    (if scale
        (glScalef (vec2d-x scale) (vec2d-y scale) 1.))

    (if color
        (glColor4f (vec4d-x color)
                   (vec4d-y color)
                   (vec4d-z color)
                   (vec4d-w color)))

    (if texture
        (image-render texture)
        (image-render-base))))

(define (generic-object-projection-matrix obj)
  (cond
   ((mesh-object? obj) (mesh-object-projection-matrix obj))
   ((2d-object? obj) (2d-object-projection-matrix obj))
   ((scene-object? obj) (scene-object-projection-matrix obj))))

(define (generic-object-render obj)
  (cond
   ((mesh-object? obj) (mesh-object-render obj))
   ((2d-object? obj) (2d-object-render obj))
   ((scene-object? obj) ((scene-object-render-proc obj) obj))))

(define (generic-object-update obj)
  (cond
   ((mesh-object? obj) (mesh-object-update-proc obj))
   ((2d-object? obj) (2d-object-update-proc obj))
   ((scene-object? obj) (scene-object-update-proc obj))))

(define scene-list '())

(define (scene-list-clear)
  (set! scene-list '()))

(define (scene-list-add obj)
  ;; keep the order according to the proejction matrix to minimize
  ;; matrix changes
  (receive (prefix tail)
      (span (lambda (el)
              (eq? (generic-object-projection-matrix el)
                   (generic-object-projection-matrix obj)))
            scene-list)
    (set! scene-list (append prefix (cons obj tail)))))

(define (scene-list-update #!optional update-fn)
  (set! scene-list
        (fold (lambda (el acc)
                (if (generic-object-update el)
                    (begin
                      ((or update-fn values) el)
                      (cons el acc))
                    acc))
              '()
              scene-list)))

(define (scene-list-render)
  ;; sort by projection matrix, do the following:
  (glMatrixMode GL_PROJECTION)
  (glLoadIdentity)
  (glMultMatrixf (scene-object-projection-matrix obj))
  (glMatrixMode GL_MODELVIEW)
  ;; then render that group ...

  (let loop ((last-matrix #f)
             (tail scene-list))
    (if (not (null? scene-list))
        (let* ((head (car tail))
               (matrix (generic-object-projection-matrix head)))
          (if (or (not last-matrix)
                  (eq? last-matrix matrix))
              (begin
                (glMatrixMode GL_PROJECTION)
                (glMultMatrixf matrix)
                (glMatrixMode GL_MODELVIEW)
                (glLoadIdentity)))
          (generic-object-render head)
          (loop matrix (cdr scene-list))))))
