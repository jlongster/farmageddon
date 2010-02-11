;;;; player
;;; Keeps track of player's statistics like life and score

;; life

(define MAX-LIFE 10)
(define LIFE MAX-LIFE)

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
            (on-death)))))

(define (life)
  LIFE)

(define (life-ratio)
  (/ LIFE MAX-LIFE))

(define (life-is-dead?)
  (<= LIFE 0))

;; goal/score

(define SCORE 0)
(define FAILED #f)

(define (score)
  SCORE)

(define (score-increase)
  (if (not (goal-met?))
      (begin
        (set! SCORE (+ SCORE 1))
        (if (goal-met?)
            (on-win)))))

(define (score-reset)
  (set! SCORE 0))

(define (goal-met?)
  (>= (score) (current-level-goal)))

(define (goal-left)
  (- (current-level-goal) (score)))

(define (goal-failed?)
  FAILED)

(define (goal-has-failed)
  (set! FAILED #t)
  (on-fail))
