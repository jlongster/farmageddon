
(scene-list-clear!)
(set-screen! level-screen)
(add-centered-mesh cow-part3-mesh)

(randomly-insert-mesh 
 (randomly-insert-mesh '(1 1) 2)
 3)

(load-randomized-fog)

(set-screen! level-screen)
(add-centered-mesh cow-part3-mesh 3.)

(begin
  (set-screen! level-screen)
  (clear-events)
  (set-difficulty! 4)
  (reset-player)
  
  (define-macro (with-alloc expr . rest)
    `(let (,expr)
       (let ((ret (begin ,@rest)))
         (free ,(car expr))
         ret)))
  
  (define-macro (install-event difficulty func)
    (if (or (< difficulty 0)
            (> difficulty 9))
        (error "define-event: invalid difficulty level ("
               difficulty
               ")"))
    (let ((difficulty (inexact->exact difficulty)))
      `(begin
         (vector-set! *events*
                      ,difficulty
                      (cons (lambda ()
                              (,func))
                            (vector-ref *events* ,difficulty))))))
  

  (install-event
   4
   (lambda ()
     (throw-attack-line
      (make-list 4 pig-mesh)
      high-gravity
      35.)
     (play-voice pig1-audio))))

(reset-player)

(begin
  (define cows
    (list
     (make-entity pig-mesh
                  (make-vec3d (random-in-range -7. 7.) -20. 40.)
                  (make-vec3d 0. (random-in-range 20. 30.) -15.)
                  low-gravity)
     (make-entity pig-mesh
                  (make-vec3d (random-in-range -7. 7.) -20. 40.)
                  (make-vec3d 0. (random-in-range 20. 30.) -15.)
                  low-gravity)
     (make-entity pig-mesh
                  (make-vec3d (random-in-range -7. 7.) -20. 40.)
                  (make-vec3d 0. (random-in-range 25. 30.) -15.)
                  low-gravity)
     (make-entity pig-mesh
                  (make-vec3d (random-in-range -7. 7.) -20. 40.)
                  (make-vec3d 0. (random-in-range 25. 30.) -15.)
                  low-gravity)
     (make-entity pig-mesh
                  (make-vec3d (random-in-range -7. 7.) -20. 40.)
                  (make-vec3d 0. (random-in-range 20. 30.) -15.)
                  low-gravity)
     (make-entity pig-mesh
                  (make-vec3d (random-in-range -7. 7.) -20. 40.)
                  (make-vec3d 0. (random-in-range 20. 30.) -15.)
                  low-gravity)))

  (scene-list-unpause)
  (set-screen! level-screen)
  (for-each scene-list-add cows)

  (thread-sleep! .5)
  
  (thread-start!
   (make-thread
    (lambda ()
      (parameterize
       ((current-perspective 3d-perspective))

       (for-each
        (lambda (cow)
          (let* ((pos (generic-object-position cow))
                 (screen (unproject (vec3d-x pos)
                                    (vec3d-y pos)
                                    (vec3d-z pos))))
            (add-dust (car screen)
                      (cadr screen)
                      (vec3d-z pos)))
          
          (explode-entity cow)
          (remove-entity cow))
        (take cows 4))

       (thread-sleep! .4)
       
       (for-each
        (lambda (cow)
          (let* ((pos (generic-object-position cow))
                 (screen (unproject (vec3d-x pos)
                                    (vec3d-y pos)
                                    (vec3d-z pos))))
            (add-dust (car screen)
                      (cadr screen)
                      (vec3d-z pos)))
          
          (explode-entity cow)
          (remove-entity cow))
        (drop cows 4)))))))
