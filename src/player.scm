;;;; player
;;; Keeps track of player's statistics like life and score

;; life

(define MAX-LIFE 15000)
(define LIFE MAX-LIFE)

(define (get-damage el)
  (let ((mesh (mesh-object-mesh el)))
    (cond
     ((eq? mesh cow-mesh) 2)
     ((eq? mesh sheep-mesh) 1)
     ((eq? mesh duck-mesh) 1)
     ((eq? mesh chicken-mesh) .5))))

(define (life-decrease! el)
  (set! LIFE
        (- LIFE (get-damage el))))

(define (life)
  LIFE)

(define (life-ratio)
  (/ LIFE MAX-LIFE))

(define (life-is-dead?)
  (<= LIFE 0))

;; goal/score

(define SCORE 0)

(define (score)
  SCORE)

(define (score-increase)
  (set! SCORE (+ SCORE 1)))

(define (score-reset)
  (set! SCORE 0))

(define (goal-met?)
  (>= (score) (current-level-goal)))

(define (goal-left)
  (- (current-level-goal) (score)))
