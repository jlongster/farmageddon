;;;; game events
;;; Implements "game" events, which make the game get harder and
;;; harder as time progresses

(set! *max-difficulty* 4)

(install-difficulties
 0 10 1 3.
 1 10 1 3.
 2 15 1 2.5
 3 15 1 3.5
 4 20 1 3.5
 5 25 1 1.
 6 25 1 .5
 7 25 1 .5
 8 25 1 .5
 9 25 1 .5)

(define low-gravity (make-vec3d 0. -11. 0.))
(define med-gravity (make-vec3d 0. -25. 0.))
(define high-gravity (make-vec3d 0. -40. 0.))

(define (random-in-range x #!optional y)
  (let* ((high (if y y x))
        (low (if y x (- x)))
        (diff (- high low)))
    (+ (* (random-real) diff) low)))

;; ------------------------------------------------------------
;; generalized events
;; these are parameterized events that happen in all
;; difficulty levels
;; ------------------------------------------------------------

(define (throw-animal-line mesh with-human? gravity force)
  (define (throw-chicken x #!optional custom-mesh)
    (add-object
     (make-entity (or custom-mesh mesh)
                  (make-vec3d x -13. 25.)
                  (make-vec3d 0. force 0.)
                  gravity)))
  (define (throw-human x)
    (throw-chicken x person-mesh))

  (let ((sel-i (if with-human?
                   (random-integer 5)
                   -1)))
    (let loop ((lst '(-6. -3. 0. 3. 6.))
               (i 0))
      (if (not (null? lst))
          (begin
            ((if (eq? sel-i i) throw-human throw-chicken)
             (car lst))
            (wait .1)
            (loop (cdr lst) (+ i 1)))))))

;; ------------------------------------------------------------
;; difficulty 0
;; only ducks and chickens in various patterns
;; ------------------------------------------------------------

;; (define (throw-chickens0)
;;   (define (throw-chicken)
;;     (add-object
;;      (make-entity chicken-mesh
;;                   (make-vec3d (random-in-range 6.)
;;                               -13.
;;                               20.)
;;                   (make-vec3d 0. 17. 0.)
;;                   low-gravity)))
;;   (throw-chicken)
;;   (wait (random-in-range 1. 2.))
;;   (throw-chicken))
;; (install-event 0 throw-chickens0)

(install-event 0
               (lambda ()
                 (throw-animal-line duck-mesh #f low-gravity 18.5)))

(define (throw-chickens2)
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
(install-event 0 throw-chickens2)

;; (define (throw-ducks0)
;;   (define (throw-duck)
;;     (add-object
;;      (make-entity duck-mesh
;;                   (make-vec3d (* (spread-number (random-real)) 6.) -13. 20.)
;;                   (make-vec3d 0. 18. 0.)
;;                   low-gravity)))
;;   (throw-duck)
;;   (wait .3)
;;   (throw-duck))
;; (install-event 0 throw-ducks0)

;; (define (throw-ducks1)
;;   (define (throw-duck)
;;     (add-object
;;      (make-entity duck-mesh
;;                   (make-vec3d (* (spread-number (random-real)) 6.) -15. 25.)
;;                   (make-vec3d 0. 18. 0.)
;;                   low-gravity)))
;;   (throw-duck)
;;   (wait .7)
;;   (throw-duck)
;;   (wait .7)
;;   (throw-duck)
;;   (wait .7))
;; (install-event 0 throw-ducks1)

(define (throw-ducks2)
  (define (throw-duck x z-vel)
    (add-object
     (make-entity duck-mesh
                  (make-vec3d x
                              -15.
                              25.)
                  (make-vec3d 0. 20. z-vel)
                  low-gravity)))
  (throw-duck 5. 5.)
  (throw-duck 2.5 0.)
  (throw-duck 0. -7.)
  (throw-duck -2.5 0.)
  (throw-duck -5. 5.))
(install-event 0 throw-ducks2)

;; ------------------------------------------------------------
;; difficulty 1
;; ducks and chickens in various orders, introduce
;; basic gameplay
;; ------------------------------------------------------------

(define (throw-ducks/chickens1)
  (define (throw-duck x z-vel)
    (add-object
     (make-entity duck-mesh
                  (make-vec3d x -16. 30.)
                  (make-vec3d 0. 30. z-vel)
                  med-gravity)))
  (define (throw-chicken x z-vel)
    (add-object
     (make-entity chicken-mesh
                  (make-vec3d x -16. 30.)
                  (make-vec3d 0. 30. z-vel)
                  med-gravity)))
  (throw-duck 4. -13.)
  (throw-duck 2. -13.)
  (throw-chicken -2. -13.)
  (throw-chicken -4. -13.))
(install-event 1 throw-ducks/chickens1)

(install-event 1
               (lambda ()
                 (throw-animal-line chicken-mesh #t med-gravity 28.)))

(define (throw-sheeps/ducks1)
  (define (throw-sheep x)
    (add-object
     (make-entity sheep-mesh
                  (make-vec3d x -13. 20.)
                  (make-vec3d 0. 30. 0.)
                  med-gravity)))
  (define (throw-duck x)
    (add-object
     (make-entity duck-mesh
                  (make-vec3d x -16. 30.)
                  (make-vec3d 0. 30. 0.)
                  med-gravity)))
  (play-voice sheep1-audio)
  (throw-sheep 0.)
  (throw-duck -4.)
  (throw-duck 4.))
(install-event 1 throw-sheeps/ducks1)

(define (throw-sheeps1)
  (define (throw-sheep x x-vel)
    (let ((obj (make-entity sheep-mesh
                            (make-vec3d x -16. 30.)
                            (make-vec3d x-vel 30. -15.)
                            med-gravity)))
      (add-object obj)))
  (play-voice sheep1-audio)
  (throw-sheep 5. -2.)
  (wait .5)
  (throw-sheep 0. 0.)
  (wait .5)
  (throw-sheep -5. 2.)
  (wait .2)
  (play-voice sheep2-audio))
(install-event 1 throw-sheeps1)

;; ------------------------------------------------------------
;; difficulty 2
;; ducks, chickens, SHEEP, and HUMANS (a few here and there)
;; ------------------------------------------------------------

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
                  (make-vec3d x -15. 20.)
                  (make-vec3d 0. 30. z-vel)
                  med-gravity)))

  (define (throw-human x z-vel)
    (throw-duck x z-vel person-mesh))
  
  ;; (throw-chicken -2.5)
  ;; (wait .5)
  ;; (throw-chicken 0.)
  ;; (wait .5)
  ;; (throw-chicken 2.5)
  
  ;; (wait 2.5)
  
  (throw-duck -2.5 -13.)
  (wait .1)
  (throw-human 0. 0.)
  (wait .1)
  (throw-duck 2.5 -13.))
(install-event 2 throw-chickens/ducks2)

(install-event 2 (lambda ()
                   (throw-animal-line chicken-mesh #t med-gravity 27.)))


;; ------------------------------------------------------------
;; difficulty 3
;; introduce COWS in addition to all the other animals
;; and humans (go minimal on the humans)
;; ------------------------------------------------------------

(define (throw-cows)
  (define (throw-cow)
    (let ((ent (make-entity cow-mesh
                            (make-vec3d
                             (* (spread-number (random-real)) 7.) -28. 40.)
                            (make-vec3d 0. 25. -11.)
                            low-gravity)))
      (add-object ent)))
  (play-voice cow-audio)
  (throw-cow)
  (throw-cow)
  (throw-cow)
  (throw-cow)
  (throw-cow))
(install-event 3 throw-cows)

;; (define-event (throw-cows3) 2
;;   (add-object
;;    (make-entity cow-mesh
;;                 (make-vec3d
;;                  (* (spread-number (random-real)) 7.) -28. 40.)
;;                 (make-vec3d 0. 25. 0.))))

;; (define-event (throw-humans) 3
;;   (add-object
;;    (make-entity person-mesh
;;                 (make-vec3d
;;                  (* (spread-number (random-real)) 7.) -28. 40.)
;;                 (make-vec3d 0. 25. 0.))))


;; ------------------------------------------------------------
;; difficulty 4
;; increase gravity, make everything go faster
;; ------------------------------------------------------------

;; ------------------------------------------------------------
;; difficulty 5
;; COW LEVEL!  all cows, everywhere!
;; ------------------------------------------------------------
