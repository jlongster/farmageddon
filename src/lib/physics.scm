;;;; "phyics"
;;; Basic physics system involving velocity & acceleration

(declare (block)
         (standard-bindings)
         (extended-bindings))

(define GRAVITY (make-vec3d 0. -25. 0.))

(define (update-physics obj)
  (let* ((now (real-time))
         (last (or (scene-object-%%last-update obj) now))
         (change (- now last)))
    (apply-acceleration obj change)
    (apply-velocity obj change)    
    (scene-object-%%last-update-set! obj now)))

(define (apply-acceleration obj change)
  (let ((velocity (scene-object-velocity obj)))
    (if velocity
        (let* ((acceleration (global-acceleration obj))
               (change (vec3d-scalar-mul acceleration change)))
          (scene-object-velocity-set!
           obj
           (vec3d-add (scene-object-velocity obj)
                      change))))))

(define (apply-velocity obj change)
  (let ((velocity (scene-object-velocity obj)))
    (if velocity
        (let ((change (vec3d-scalar-mul velocity change)))
          (scene-object-position-set!
           obj
           (vec3d-add (scene-object-position obj)
                      change))))))

(define (global-acceleration obj)
  ;; apply gravity
  (let ((accel (scene-object-acceleration obj)))
    (if accel
        (vec3d-add accel GRAVITY)
        GRAVITY)))

(define (kick v)
  (for-each (lambda (el)
              (if (scene-object-velocity el)
                  (scene-object-velocity-set!
                   el
                   (vec3d-add v (scene-object-velocity el)))))
            scene-list))
