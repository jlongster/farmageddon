;;;; "scene"
;;; Functionality for organizing data in a scene in a heirarchal
;;; order and rendering it

;; (define-class Scene-Object Object
;;   (mesh
;;    (= color :maybe-uninitialized)
;;    (= position :initializer (lambda () (make-vec3d 0. 0. 0.)))
;;    (= rotation :initializer (lambda () (make-vec4d 0. 0. 0. 0.)))
;;    (= scale :initializer (lambda () (make-vec3d 1. 1. 1.)))))

;; (define-class Physics-Object Scene-Object
;;   (= velocity :initialize (lambda () (make-vec3d 0. 0. 0.)))
;;   (= acceleration :initialize (lambda () (make-vec3d 0. 0. 0.)))
;;   (= updater :initialize )
;;   (= %%last-update :initializer (lambda () #f)))

(declare (block)
         (standard-bindings)
         (extended-bindings))

(define-type scene-object
  constructor: really-make-scene-object
  mesh
  color
  position
  rotation
  radius
  velocity
  acceleration
  callback
  %%last-update
  data
  voice-source
  thud-source)

(define (make-scene-object mesh color pos #!optional rot scale vel accel update data)
  (really-make-scene-object mesh color pos rot scale vel accel
                            (or update (lambda (el) #t))
                            #f
                            data
                            #f
                            #f))

(define scene-list '())

(define (scene-list-clear)
  (set! scene-list '()))

(define (scene-list-add obj)
  (set! scene-list (cons obj scene-list)))

(define (scene-list-update #!optional update-fn)
  (set! scene-list
        (fold (lambda (el acc)
                (if ((scene-object-callback el) el)
                    (begin
                      ((or update-fn values) el)
                      (cons el acc))
                    acc))
              '()
              scene-list)))

(define (scene-list->render-queue)
  (map (lambda (el)
         (lambda ()
           (let ((mesh (scene-object-mesh el))
                 (color (scene-object-color el))
                 (pos (scene-object-position el))
                 (rot (scene-object-rotation el))
                 (vel (scene-object-velocity el))
                 (radius (scene-object-radius el)))

             (glVertexPointer 3 GL_FLOAT 0 (obj-vertices mesh))
             (glEnableClientState GL_VERTEX_ARRAY)
             (glNormalPointer GL_FLOAT 0 (->void-array (obj-normals mesh)))
             (glEnableClientState GL_NORMAL_ARRAY)

             (reset-camera)

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

             (if radius
                 (glScalef radius radius radius))

             (for-each (lambda (chunk)
                         (if (obj-chunk-mat chunk)
                             (let* ((mat (obj-chunk-mat chunk))
                                    (d (material-diffuse mat)))
                               (glMaterialfv GL_FRONT_AND_BACK
                                             GL_DIFFUSE
                                             (vector->float-array
                                              (vector
                                               (vec3d-x d) (vec3d-y d) (vec3d-z d)
                                               1.)))
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

             (glDisableClientState GL_NORMAL_ARRAY))))
       scene-list))

;;;;; render queue
(define (run-render-queue rq)
  (for-each (lambda (el) (el)) rq))
