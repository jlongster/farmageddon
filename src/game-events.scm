;;;; game events
;;; Implements "game" events, which make the game get harder and
;;; harder as time progresses

(declare (block)
         (standard-bindings)
         (extended-bindings))

(set! *max-difficulty* 9)

(install-difficulties
 0 5 0  2.75   2.
 1 8 0  2.25   1.5
 2 10 1 2.   1.2
 3 20 1 1.5  1.
 4 25 2 1.   1.25
 5 10 0 1.   1.
 6 25 3 1.   1.
 7 20 4 .85   1.5
 8 25 5 .75   1.25
 9 25 6 .75   1.)

(define low-gravity (make-vec3d 0. -11. 0.))
(define med-gravity (make-vec3d 0. -25. 0.))
(define high-gravity (make-vec3d 0. -40. 0.))

;; util
;;
;; these functions help construct lists of meshes to be used in
;; formations

(define (random-mesh #!optional lst)
  (let ((meshes (or lst
                    (list cow-mesh
                          sheep-mesh
                          chicken-mesh
                          duck-mesh
                          pig-mesh))))
    (list-ref meshes (random-integer (length meshes)))))

(define (randomly-insert-mesh meshes mesh)
  (let ((sel-i (random-integer (+ 1 (length meshes)))))
    (let loop ((tail meshes)
               (acc '())
               (i 0))
      (if (null? tail)
          (if (eq? sel-i i)
              (reverse (cons mesh acc))
              (reverse acc))
          (let ((head (car tail)))
            (loop (cdr tail)
                  (cons head 
                        (if (eq? i sel-i)
                            (cons mesh acc)
                            acc))
                  (+ i 1)))))))

(define (vector-range start end count #!optional inclusive?)
  (let* ((line (vec3d-sub end start))
         (step (vec3d-scalar-mul line (exact->inexact (/ count)))))
    (let loop ((acc '())
               (i 0))
      (if (>= i count)
          (reverse
           (if inclusive? (cons end acc) acc))
          (loop (cons (vec3d-add start
                                 (vec3d-scalar-mul step (exact->inexact i)))
                      acc)
                (+ i 1))))))

(define (make-nuke mesh)
  (cons mesh 'nuke))

(define (maybe-nuke-mesh obj)
  (if (pair? obj)
      (car obj)
      obj))

(define (nuke? obj)
  (and (pair? obj)
       (eq? (cdr obj) 'nuke)))

;; formations
;;
;; these are general formations which animals organize in to
;; attack with

(define (throw-animal-line meshes gravity force)
  (define (throw mesh x)
    (add-object
     (make-entity (maybe-nuke-mesh mesh)
                  (make-vec3d x -16. 25.)
                  (make-vec3d 0. force 0.)
                  gravity
                  (nuke? mesh))))

  (let* ((len (length meshes))
         (x-min -6.)
         (x-max 6.)
         (w (- x-max x-min))
         (step (/ w (- (max len 2) 1))))
    (let loop ((tail meshes)
               (i 0))
      (if (not (null? tail))
          (let ((head (car tail)))
            (throw head (+ x-min (* step i)))
            (wait .1)
            (loop (cdr tail)
                  (+ i 1)))))))

(define (throw-attack-line meshes gravity force)
  (define (throw mesh x)
    (add-object
     (make-entity (maybe-nuke-mesh mesh)
                  (make-vec3d x -16. 25.)
                  (make-vec3d 0. force -16.)
                  gravity
                  (nuke? mesh))))

  (let* ((len (length meshes))
         (x-min -4.)
         (x-max 4.)
         (w (- x-max x-min))
         (step (/ w (- (max len 2) 1))))
    (let loop ((tail meshes)
               (i 0))
      (if (not (null? tail))
          (let ((head (car tail)))
            (throw head (+ x-min (* step i)))
            (wait .1)
            (loop (cdr tail)
                  (+ i 1)))))))

(define (throw-square meshes gravity)
  (define (throw-mesh mesh x y-vel z-vel)
    (add-object
     (make-entity (maybe-nuke-mesh mesh)
                  (make-vec3d x -16. 30.)
                  (make-vec3d 0. y-vel z-vel)
                  gravity
                  (nuke? mesh))))
  (if (eq? gravity med-gravity)
      (begin
        (throw-mesh (first meshes) 2.5 32.6 -17.)
        (throw-mesh (second meshes) -2.5 32.6 -17.)
        (throw-mesh (third meshes) 2.5 25. -17.)
        (throw-mesh (fourth meshes) -2.5 25. -17.))
      (begin
        (throw-mesh (first meshes) 2.5 42.2 -17.)
        (throw-mesh (second meshes) -2.5 42.2 -17.)
        (throw-mesh (third meshes) 2.5 34. -17.)
        (throw-mesh (fourth meshes) -2.5 34. -17.))))

(define (throw-v meshes gravity y-vel z-vel)
  (define (throw mesh pos)
    (add-object
     (make-entity (maybe-nuke-mesh mesh)
                  pos
                  (make-vec3d (/ (- (vec3d-x pos)) 4.) y-vel z-vel)
                  gravity
                  (nuke? mesh))))

  (let ((count (length meshes)))
    (if (not (odd? count))
        (error "throw-v: must be odd number of meshes"))

    (let* ((half (inexact->exact (floor (/ count 2))))
           (line1 (vector-range (make-vec3d -5. -20. 35.)
                                (make-vec3d 0. -20. 30.)
                                half))
           (line2 (vector-range (make-vec3d 0. -20. 30.)
                                (make-vec3d 5. -20. 35.)
                                half
                                #t)))
      (for-each (lambda (mesh-cell)
                  (throw (cadr mesh-cell)
                         (car mesh-cell)))
                (zip line1 (take meshes half)))
      (for-each (lambda (mesh-cell)
                  (throw (cadr mesh-cell)
                         (car mesh-cell)))
                (zip line2 (drop meshes half))))))

(define (throw-2 meshes gravity y-vel z-vel)
  (define (throw mesh x x-vel)
    (let ((obj (make-entity (maybe-nuke-mesh mesh)
                            (make-vec3d x -16. 30.)
                            (make-vec3d x-vel y-vel z-vel)
                            gravity
                            (nuke? mesh))))
      (add-object obj)))
  (throw (first meshes) -4. 1.)
  (throw (second meshes) 4. -1.))

(define (throw-3 meshes gravity y-vel z-vel)
  (define (throw mesh x x-vel)
    (let ((obj (make-entity (maybe-nuke-mesh mesh)
                            (make-vec3d x -16. 30.)
                            (make-vec3d x-vel y-vel z-vel)
                            gravity
                            (nuke? mesh))))
      (add-object obj)))
  (throw (first meshes) 5. -2.)
  (wait .1)
  (throw (second meshes) 0. 0.)
  (wait .1)
  (throw (third meshes) -5. 2.))

(define (throw-random-nuke)
  (add-object
   (make-entity (random-mesh)
                (make-vec3d (random-in-range -4. 4.) -20. 30.)
                (make-vec3d 0. 40. -20.)
                high-gravity
                #t)))

(define (throw-random-healer)
  (add-object
   (make-entity (random-mesh (list steak-mesh bones-mesh))
                (make-vec3d (random-in-range -4. 4.) -10. 20.)
                (make-vec3d 0. 35. 0.)
                high-gravity)))

;; ------------------------------------------------------------
;; difficulty 0
;; only ducks and chickens in various patterns
;; ------------------------------------------------------------

(define (throw-ducks0)
  (define (throw-duck)
    (add-object
     (make-entity duck-mesh
                  (make-vec3d (random-in-range 6.)
                              -13.
                              20.)
                  (make-vec3d 0. 17. 0.)
                  low-gravity)))
  (throw-duck)
  (play-voice duck1-audio)
  (wait (random-in-range .3 1.3))
  (throw-duck)
  (play-voice duck1-audio))
(install-event 0 throw-ducks0)

(define (throw-chickens0)
  (define (throw-chicken x z-vel)
    (add-object
     (make-entity chicken-mesh
                  (make-vec3d x -13. 20.)
                  (make-vec3d 0. 18.5 z-vel)
                  low-gravity)))
  (play-voice chicken1-audio)
  (throw-chicken -2. -5.)
  (throw-chicken 0. 0.)
  (throw-chicken 2. 15.)
  (wait .2)
  (play-voice chicken2-audio))
(install-event 0 throw-chickens0)

(install-event
 0
 (lambda ()
   (throw-v (make-list 5 chicken-mesh) low-gravity 22. -12.)
   (play-voice chicken2-audio)
   (wait .15)
   (play-voice chicken1-audio)))

(install-event
 0
 (lambda ()
   (throw-animal-line
    (make-list 4 duck-mesh)
    low-gravity
    18.5)
   (play-voice duck1-audio)
   (wait .6)
   (play-voice duck1-audio)))

;; ------------------------------------------------------------
;; difficulty 1
;; ducks, chickens, SHEEP, HUMANS in various orders, introduce
;; basic gameplay
;; ------------------------------------------------------------

(install-event
 1
 (lambda ()
   (throw-square
    (make-list 4 duck-mesh)
    med-gravity)
   (play-voice duck1-audio)
   (wait .4)
   (play-voice duck1-audio)))

(install-event
 1
 (lambda ()
   (throw-3 (make-list 3 sheep-mesh) med-gravity 30. -15.)
   (play-voice sheep1-audio)
   (wait .1)
   (play-voice sheep2-audio)))

(install-event
 1
 (lambda ()
   (throw-2 (make-list 2 sheep-mesh) med-gravity 30. -15.)
   (play-voice sheep1-audio)))

(install-event
 1
 (lambda ()
   (throw-v (make-list 5 chicken-mesh)
            med-gravity
            34.
            -14.)
   (play-voice chicken1-audio)
   (wait .1)
   (play-voice chicken2-audio)))

(define (throw-ducks/chickens1)
  (define (throw-mesh mesh x y-vel z-vel)
    (add-object
     (make-entity mesh
                  (make-vec3d x -16. 30.)
                  (make-vec3d 0. y-vel z-vel)
                  med-gravity)))
  (play-voice chicken1-audio)
  (throw-mesh duck-mesh 2. 30. -20.)
  (throw-mesh duck-mesh 1.1 30. -20.)
  (throw-mesh chicken-mesh -1.1 30. -20.)
  (throw-mesh chicken-mesh -2. 30. -20.)
  (wait .2)
  (play-voice duck1-audio))
(install-event 1 throw-ducks/chickens1)

;; ------------------------------------------------------------
;; difficulty 2
;; ducks, chickens, sheep, humans, PIGS (a few here and there)
;; ------------------------------------------------------------

(install-event 2 make-fog)

(install-event
 2
 (lambda ()
   (throw-animal-line
    (randomly-insert-mesh (make-list 3 chicken-mesh)
                          person-mesh)
    med-gravity
    28.)
   (play-voice chicken2-audio)
   (wait .3)
   (play-voice chicken1-audio)))

(install-event
 2
 (lambda ()
   (throw-3 (make-list 3 pig-mesh) med-gravity 30. -15.)
   (play-voice pig1-audio)))

(install-event
 2
 (lambda ()
   (throw-2 (list pig-mesh sheep-mesh) med-gravity 28. -15.)
   (play-voice pig1-audio)
   (play-voice sheep2-audio)))

(define (throw-chickens/ducks2)
  (define (throw-chicken x)
    (add-object
     (make-entity chicken-mesh
                  (make-vec3d x -10. 15.)
                  (make-vec3d 0. 28. 0.)
                  med-gravity)))

  (define (throw-duck x z-vel #!optional mesh)
    (add-object
     (make-entity (or mesh duck-mesh)
                  (make-vec3d x -15. 30.)
                  (make-vec3d 0. 30. z-vel)
                  med-gravity)))

  (define (throw-human x z-vel)
    (throw-duck x z-vel person-mesh))

  (play-voice chicken1-audio)
  (throw-duck -2.5 -13.)
  (wait .1)
  (throw-human 0. 0.)
  (wait .1)
  (throw-duck 2.5 -13.)
  (play-voice duck1-audio))
(install-event 2 throw-chickens/ducks2)

;; ------------------------------------------------------------
;; difficulty 3
;; introduce COWS in addition to all the other animals
;; and humans (go minimal on the humans)
;; ------------------------------------------------------------

(install-event 3 throw-random-healer)

(install-event 3 make-fog)

(install-event
 3
 (lambda ()
   (throw-animal-line
    (randomly-insert-mesh (make-list 4 chicken-mesh)
                          person-mesh)
    med-gravity
    27.)
   (play-voice chicken1-audio)
   (wait .2)
   (play-voice chicken2-audio)))

(install-event
 3
 (lambda ()
   (throw-v
    (randomly-insert-mesh (list sheep-mesh chicken-mesh)
                          cow-mesh)
    med-gravity
    34.
    -14.)
   (play-voice chicken1-audio)
   (play-voice sheep1-audio)))

(install-event
 3
 (lambda ()
   (define (throw-human)
     (add-object
      (make-entity person-mesh
                   (make-vec3d (random-in-range -7. 7.) -15. 20.)
                   (make-vec3d 0. 30. 0.)
                   med-gravity)))
   (throw-2
    (randomly-insert-mesh (list chicken-mesh)
                          cow-mesh)
    med-gravity 30. -14.)
   (wait .2)
   (throw-human)
   (play-voice chicken1-audio)
   (play-voice cow2-audio)))

;; ------------------------------------------------------------
;; difficulty 4
;; increase gravity, make everything go faster
;; ------------------------------------------------------------

(install-event 4 make-fog)

(install-event
   4
   (lambda ()
     (throw-attack-line
      (make-list 3 pig-mesh)
      high-gravity
      40.)
     (play-voice pig1-audio)))

(install-event
   4
   (lambda ()
     (throw-square
      (randomly-insert-mesh (make-list 3 chicken-mesh)
                            person-mesh)
      high-gravity)
     (play-voice chicken2-audio)))

(install-event
 4
 (lambda ()
   (throw-v
    (make-list 3 sheep-mesh)
    med-gravity
    34.
    -14.)
   (play-voice sheep1-audio)))

(install-event
   4
   (lambda ()
     (throw-attack-line (make-list 5 duck-mesh)
                        high-gravity
                        38.)
     (play-voice duck1-audio)))

(install-event
   4
   (lambda ()
     (add-object
      (make-entity cow-mesh
                   (make-vec3d 0. -20. 30.)
                   (make-vec3d 0. 40. -20.)
                   high-gravity))
     (play-voice cow2-audio)))

(install-event
   4
   (lambda ()
     (throw-attack-line (make-list 5 chicken-mesh)
                        high-gravity
                        38.)
     (play-voice chicken2-audio)))

;; ------------------------------------------------------------
;; difficulty 5
;; COW LEVEL!  all cows, everywhere!
;; ------------------------------------------------------------

(install-event
 5
 (lambda ()
   (throw-attack-line
    (make-list 7 sheep-mesh)
    med-gravity
    34.)
   (play-voice sheep1-audio)
   (wait .2)
   (throw-random-nuke)))

(install-event 5 make-fog)

(install-event
 5
 (lambda ()
   (throw-v
    (make-list 3 cow-mesh)
    high-gravity
    42.
    -18.)
   (play-audio cow2-audio)
   (wait .2)
   (throw-random-nuke)
   (wait .1)
   (play-audio cow1-audio)))

(install-event
 5
 (lambda ()
   (throw-animal-line
    (make-list 4 cow-mesh)
    high-gravity
    40.)
   (play-audio cow2-audio)
   (play-audio cow1-audio)))

;; ------------------------------------------------------------
;; difficulty 6
;; what now?
;; ------------------------------------------------------------

(install-event 6 throw-random-healer)
