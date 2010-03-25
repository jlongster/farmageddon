
(begin
  (set-screen! level-screen)
  (let loop ((i 0))
    (if (< i 25)
        (let* ((pos (make-vec3d (random-in-range 5. 20.)
                                (random-in-range -5. 5.)
                                (random-in-range 3. 5.))))
          (scene-list-add
           (make-2d-object
            3d-perspective
            position: pos
            color: (make-vec4d 1. 1. 1. 1.)
            texture: fog-texture
            scale: (make-vec3d (random-in-range 2. 5.)
                               (random-in-range 2. 5.)
                               (random-in-range 2. 5.))
            update: (lambda (this)
                      (let ((pos (2d-object-position this)))
                        (if (> (vec3d-x pos) -10.)
                            (vec3d-x-set! pos (- (vec3d-x pos) .05))
                            (scene-list-remove this))))))
          (loop (+ i 1))))))
