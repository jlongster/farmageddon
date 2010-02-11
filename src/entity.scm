;;;; entity
;;; Entites are scene object types which have a 3d mesh association
;;; and are thrown at the player's screen.  Implemented as the type
;;; `mesh-object` in the scene list.

(define SCREEN-DEPTH 10.)

;; making and throwing

(define ENTITY-MAX-DEPTH 40.)
(define ENTITY-SCALE 4.)

(define (%%get-random-time)
  (+ (real-time) (* (random-real)
                    (or (current-animal-frequency) 2.5))))

(define %%next-time #f)

(define (definitely-make-entity)
  (scene-list-add (make-entity)))

(define (possibly-make-entity)
  (if %%next-time
      (if (> (real-time) %%next-time)
          (begin
            (definitely-make-entity)
            (set! %%next-time (%%get-random-time))))
      (set! %%next-time (%%get-random-time))))

(define (random-mesh)
  (let ((meshes (cons person-mesh
                      (or (current-available-meshes)
                          (list cow-mesh
                                sheep-mesh
                                chicken-mesh
                                duck-mesh)))))
    (list-ref meshes (random-integer (length meshes)))))

(define (make-entity)
  (let* ((mesh (random-mesh))
         (pos (make-vec3d
               (* (spread-number (random-real)) 7.) -28. ENTITY-MAX-DEPTH))
         (to-eye (vec3d-unit (vec3d-sub (make-vec3d 0. 0. 0.)
                                        pos)))
         (x (* (spread-number (random-real)) 3.16))
         (thrust (+ 15. (* x x)))
         (vel (make-vec3d (* (vec3d-x to-eye) thrust)
                          (+ 25.5 (spread-number (random-real)))
                          (if (eq? mesh person-mesh)
                              (* (vec3d-z to-eye) (/ thrust 2.5))
                              (* (vec3d-z to-eye) thrust)))))
    (let ((obj (make-mesh-object
                3d-perspective
                mesh: mesh
                position: pos
                rotation: (make-vec4d (random-real)
                                      (random-real)
                                      0.
                                      1.)
                scale: (make-vec3d ENTITY-SCALE ENTITY-SCALE ENTITY-SCALE)
                color: (make-vec4d 1. 1. 1. 1.)
                velocity: vel
                update: (let ((speed (* (random-real) 4.)))
                          (lambda (this)
                            (mesh-object-rotation-set!
                             this
                             (vec4d-add (mesh-object-rotation this)
                                        (make-vec4d 0. 0. 0. speed)))
                            (let* ((pos (mesh-object-position this))
                                   (screen-y (cadr (unproject (vec3d-x pos)
                                                              (vec3d-y pos)
                                                              (vec3d-z pos))))
                                   (screen-height (UIView-height (current-view))))
                              (if (> screen-y (+ screen-height 100))
                                  (remove-entity this))))))))
      ;;(play-voice-for-entity obj)
      (mesh-object-data-set! obj (get-next-color-index obj))
      obj)))

(define (remove-entity obj)
  (release-color-index (mesh-object-data obj))
  (on-entity-remove obj)
  (scene-list-remove obj))

;; the level pump which implements the cracking of the screen and all
;; associated events

(define (global-update el)
  (if (mesh-object? el)
      (begin
        (update-physics el)
        (update-audio el)
        (let* ((pos (mesh-object-position el))
               (coords (unproject (vec3d-x pos)
                                  (vec3d-y pos)
                                  (vec3d-z pos)))
               (width (UIView-width (current-view)))
               (height (UIView-height (current-view))))
          (if (and (< (vec3d-z pos) SCREEN-DEPTH)
                   (>= (car coords) 0)
                   (>= (cadr coords) 0)
                   (< (car coords) width)
                   (< (cadr coords) height))
              (begin
                (life-decrease! el)
                (vec3d-z-set! pos SCREEN-DEPTH)
                (apply crack coords)
                (play-thud-for-entity el)
                (mesh-object-velocity-set! el (make-vec3d 0. 0. 0.))
                (mesh-object-acceleration-set! el (make-vec3d 0. -10. 0.))))))))

;; explosions

(define (explode-cow obj)
  (define (body-part-direction obj)
    (let* ((mesh (mesh-object-mesh obj))
           (rot (mesh-object-rotation obj))
           (box (obj-bounding-box mesh))
           (x (/ (+ (bounding-box-min-x box)
                    (bounding-box-max-x box)) 2.))
           (y (/ (+ (bounding-box-min-y box)
                    (bounding-box-max-y box)) 2.))
           (z (/ (+ (bounding-box-min-z box)
                    (bounding-box-max-z box)) 2.)))
      (quaternion-rotate
       (quaternion-axisangle (vec4d-w rot)
                             (vec4d-x rot)
                             (vec4d-y rot)
                             (vec4d-z rot))
       (vec3d-unit (make-vec3d x y z)))))

  (let* ((mesh (mesh-object-mesh obj))
         (obj1 (copy-mesh-object obj))
         (obj2 (copy-mesh-object obj))
         (parts (list cow-part1-mesh
                      cow-part2-mesh
                      cow-part3-mesh))
         (part1 (list-ref parts
                          (random-integer (length parts))))
         (parts (delete part1 parts))
         (part2 (list-ref parts
                          (random-integer (length parts)))))

    (mesh-object-mesh-set! obj1 part1)
    (let ((dir (body-part-direction obj1)))
      (mesh-object-velocity-set! obj1 (vec3d-add
                                       (vec3d-scalar-mul dir 15.)
                                       (mesh-object-velocity obj)))
      (mesh-object-data-set! obj1 (get-next-color-index obj1)))

    (mesh-object-mesh-set! obj2 part2)
    (let ((dir (body-part-direction obj2)))
      (mesh-object-velocity-set! obj2 (vec3d-add
                                       (vec3d-scalar-mul dir 15.)
                                       (mesh-object-velocity obj)))
      (mesh-object-data-set! obj2 (get-next-color-index obj2)))
    
    (scene-list-add obj1)
    (scene-list-add obj2)))

(define (explode-generic obj)
  #f)

(define (explode-entity obj)
  (let ((mesh (mesh-object-mesh obj)))
    (cond
     ((eq? mesh cow-mesh) (explode-cow obj))
     (else (explode-generic obj)))))

;; audio

(define (update-audio obj)
  (define (saturate n)
    (min 1. (max 0. n)))

  (let ((source (mesh-object-voice-source obj))
        (pos (mesh-object-position obj)))
    #t
    #;
    (alSourcef source
               AL_GAIN
               (- 1. (saturate
                      (/ (- (vec3d-z pos) %%screen-depth) 40.))))))

(define (play-voice-for-entity obj)
  (let* ((mesh (mesh-object-mesh obj))
         (buffer
          (cond
           ((eq? cow-mesh mesh) moo-audio)
           ((eq? sheep-mesh mesh) bah-audio)
           ((eq? chicken-mesh mesh) chicken-audio)
           (else #f))))
    (if buffer
        (let ((source (make-audio-source buffer)))
          (play-audio source)
          (mesh-object-voice-source-set! obj source)))))

(define (play-thud-for-entity obj)
  (let ((source (make-audio-source thud-audio)))
    (play-audio source)
    (mesh-object-thud-source-set! obj source)))

;; removing and killing events

(define (on-entity-remove obj)
  (let ((voice-source (mesh-object-voice-source obj))
        (thud-source (mesh-object-thud-source obj)))
    (if thud-source
        (begin
          (stop-audio thud-source)
          (free-audio-source thud-source)))

    (if voice-source
        (begin
          (stop-audio voice-source)
          (free-audio-source voice-source))))
  (mesh-object-voice-source-set! obj #f)
  (mesh-object-thud-source-set! obj #f))

(define (on-entity-kill obj)
  (explode-entity obj)

  (if (eq? person-mesh
           (mesh-object-mesh obj))
      (goal-has-failed)
      (score-increase)))
