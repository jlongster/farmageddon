
(define *fog-audio-source* #f)
(define *fog-list* '())

(define (fog-list-clear!)
  (set! *fog-list* '())
  (if *fog-audio-source*
      (begin
        (stop-audio *fog-audio-source*)
        (rewind-audio *fog-audio-source*))))

(define (fog-list-add obj)
  (set! *fog-list* (cons obj *fog-list*)))

(define (make-fog)
  (if (null? *fog-list*)
      (begin
        (let loop ((i 0))
          (if (< i 6)
              (let* ((pos (make-vec3d (random-in-range 10.6 25.)
                                      0.
                                      (random-in-range 9. 10.))))
                (fog-list-add
                 (make-2d-object
                  3d-perspective
                  position: pos
                  color: (make-vec4d 1. 1. 1. 1.)
                  texture: fog-texture
                  scale: (make-vec3d (random-in-range 6. 7.)
                                     (random-in-range 6. 7.)
                                     (random-in-range 6. 7.))
                  rotation: (make-vec4d 0. 0. 1. (random-in-range 0. 360.))
                  update: (let ((then (real-time))
                                (dist (- (vec3d-x pos) -10)))
                            (lambda (this)
                              (let ((pos (2d-object-position this)))
                                (if (> (vec3d-x pos) -10.)
                                    (vec3d-x-set! pos (- (vec3d-x pos)
                                                         (* (- (real-time) then) .01)))
                                    (scene-list-remove this)))))))
                (loop (+ i 1)))))
        (play-audio *fog-audio-source*))))

(define (render-fog)
  (load-perspective 3d-perspective)
  (2d-object-prerender #t)
  (for-each (lambda (obj)
              (2d-object-render obj))
            *fog-list*)
  (2d-object-postrender #t))

(define (update-fog)
  (set! *fog-list* (scene-list-update #f #f *fog-list*)))

(define (fog-init)
  (set! *fog-audio-source* (make-audio-source fog-audio)))
