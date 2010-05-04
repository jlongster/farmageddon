;;;; "phyics"
;;; Basic physics system involving velocity & acceleration

(declare (block)
         (standard-bindings)
         (extended-bindings))

(define (update-physics obj)
  (let* ((now (real-time))
         (last (or (mesh-object-last-update obj) now))
         (change (- now last)))
    (apply-acceleration obj change)
    (apply-velocity obj change)
    (mesh-object-last-update-set! obj now)))

(define (apply-acceleration obj change)
  (let ((velocity (mesh-object-velocity obj))
        (acceleration (mesh-object-acceleration obj)))
    (if (and velocity acceleration)
        (begin
          (vec3d-x-set! velocity
                        (+ (vec3d-x velocity)
                           (* (vec3d-x acceleration) change)))
          
          (vec3d-y-set! velocity
                        (+ (vec3d-y velocity)
                           (* (vec3d-y acceleration) change)))
          
          (vec3d-z-set! velocity
                        (+ (vec3d-z velocity)
                           (* (vec3d-z acceleration) change)))))))

(define (apply-velocity obj change)
  (let ((velocity (mesh-object-velocity obj)))
    (if velocity
        (let ((pos (mesh-object-position obj)))
          (vec3d-x-set! pos (+ (vec3d-x pos)
                               (* (vec3d-x velocity) change)))

          (vec3d-y-set! pos (+ (vec3d-y pos)
                               (* (vec3d-y velocity) change)))

          (vec3d-z-set! pos (+ (vec3d-z pos)
                               (* (vec3d-z velocity) change)))))))

(define (kick v)
  (for-each (lambda (el)
              (if (mesh-object-velocity el)
                  (mesh-object-velocity-set!
                   el
                   (vec3d-add v (mesh-object-velocity el)))))
            scene-list))
