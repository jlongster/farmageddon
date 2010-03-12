;;;; player
;;; Keeps track of player's statistics like life and score

(declare (block)
         (standard-bindings)
         (extended-bindings))

;; life

(define LIFE #f)

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
              (- LIFE (get-damage el))))))

(define (life)
  LIFE)

(define (life-ratio)
  (/ LIFE (current-max-life)))

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
        (on-score-increase))))

(define (reset-score)
  (set! SCORE 0))

(define (goal-met?)
  (and (eq? (current-goal-type) 'goal)
       (>= (score) (current-level-goal))))

(define (goal-left)
  (and (eq? (current-goal-type) 'goal)
       (- (current-level-goal) (score))))

;; clock

(define TIMER #f)

(define (start-timer)
  (set! TIMER (real-time))
  (set! SIGNALLED #f))

(define (timer-met?)
  (and (eq? (current-goal-type) 'timer)
       (>= (- (real-time) TIMER)
           ;; we add .1 seconds to let the overlay updates run through
           ;; so that the current time is rendered when the level is
           ;; completed
           (+ (current-level-goal) .1))))

(define (timed-out?)
  (and (eq? (current-goal-type) 'timed)
       (>= (- (real-time) TIMER)
           (current-level-goal))))

(define (timer-status)
  (if (eq? (current-goal-type) 'timed)
      (max 0
           (inexact->exact
            (- (current-level-goal)
               (floor (- (real-time) TIMER)))))
      (inexact->exact
       (floor (- (real-time) TIMER)))))

;; player

(define SIGNALLED #f)

(define (player-init)
  (start-timer)
  (set! LIFE (current-max-life)))

(define (player-update)
  (define (done)
    (on-complete)
    (set! SIGNALLED #t))
  
  (if (not SIGNALLED)
      (cond
       ((goal-met?)
        (on-win)
        (done))

       ((life-is-dead?)
        (on-death)
        (on-fail)
        (done))
       
       ((player-failed?)
        (on-fail)
        (done))

       ((or (timer-met?)
            (timed-out?))
        (if (timer-met?)
            (on-win)
            (on-fail))
        (done)))))

(define (player-has-failed)
  (set! FAILED #t))

(define (player-failed?)
  (or FAILED
      (life-is-dead?)
      (timed-out?)))

(define (player-finished?)
  (or (goal-met?)
      (timer-met?)
      (player-failed?)))

(define (reset-player)
  (set! FAILED #f)
  (reset-score)
  (reset-cracks))
