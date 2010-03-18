;;;; "phyics"
;;; Basic physics system involving velocity & acceleration

(declare (block)
         (standard-bindings)
         (extended-bindings))

(define GRAVITY (make-vec3d 0. 0. 0.))

(define (update-physics obj)
  (let* ((now (real-time))
         (last (or (mesh-object-last-update obj) now))
         (change (- now last)))
    (apply-acceleration obj change)
    (apply-velocity obj change)    
    (mesh-object-last-update-set! obj now)))

(define (apply-acceleration obj change)
  (let ((velocity (mesh-object-velocity obj)))
    (if velocity
        (let* ((acceleration (global-acceleration obj))
               (change (vec3d-scalar-mul acceleration change)))
          (mesh-object-velocity-set!
           obj
           (vec3d-add (mesh-object-velocity obj)
                      change))))))

(define (apply-velocity obj change)
  (let ((velocity (mesh-object-velocity obj)))
    (if velocity
        (let ((change (vec3d-scalar-mul velocity change)))
          (mesh-object-position-set!
           obj
           (vec3d-add (mesh-object-position obj)
                      change))))))

(define (global-acceleration obj)
  ;; apply gravity
  (let ((accel (mesh-object-acceleration obj)))
    (if accel
        (vec3d-add accel GRAVITY)
        GRAVITY)))

(define (kick v)
  (for-each (lambda (el)
              (if (mesh-object-velocity el)
                  (mesh-object-velocity-set!
                   el
                   (vec3d-add v (mesh-object-velocity el)))))
            scene-list))
