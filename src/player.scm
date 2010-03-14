;;;; player
;;; Keeps track of player's statistics like life and score

(declare (block)
         (standard-bindings)
         (extended-bindings))

;; life

(define LIFE #f)
(define VICTOR #f)

(define (get-damage el)
  (let ((mesh (mesh-object-mesh el)))
    (cond
     ((eq? mesh cow-mesh) 2)
     ((eq? mesh chicken-mesh) .5)
     (else 1))))

(define (life-decrease! el)
  (if (not (life-is-dead?))
      (begin
        (set! LIFE
              (- LIFE (get-damage el)))
        (if (life-is-dead?)
            (set! VICTOR el)))))

(define (life)
  LIFE)

(define (life-ratio)
  (/ LIFE (current-max-life)))

(define (life-is-dead?)
  (<= LIFE 0))

;; goal/score

(define SCORE 0)

(define (score)
  SCORE)

(define (score-increase pts)
  (set! SCORE (+ SCORE pts))
  (on-score-increase))

(define (reset-score)
  (set! SCORE 0))

;; player

(define FAILED #f)
(define SIGNALLED #f)

(define (player-update)
  (define (done)
    (set! SIGNALLED #t))
  
  (if (not SIGNALLED)
      (cond
       ((life-is-dead?)
        (on-death)
        (done))
       
       ((player-failed?)
        (on-fail)
        (done)))))

(define (player-has-failed)
  (set! FAILED #t))

(define (player-animal-victor)
  VICTOR)

(define (player-failed?)
  (or FAILED
      (life-is-dead?)))

(define (player-finished?)
  (player-failed?))

(define (reset-player)
  (set! LIFE (current-max-life))
  (set! FAILED #f)
  (set! SIGNALLED #f)
  (reset-score)
  (reset-cracks)
  (score-setup))
