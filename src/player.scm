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
        (background-pop (make-vec4d 1. .5 .5 1.))
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

(define KILL-COUNT 0)
(define SCORE 0)

(define (score)
  SCORE)

(define (kill-count)
  KILL-COUNT)

(define (score-increase obj)
  (set! KILL-COUNT (+ KILL-COUNT 1))
  (let ((pts (entity-points obj)))
    (set! SCORE (+ SCORE pts))
    (on-score-increase)))

(define (reset-score)
  (set! SCORE 0))

(define (reset-kill-count)
  (set! KILL-COUNT 0))

(define (make-persistent-score name score)
  (cons name score))

(define (persistent-score-name score)
  (car score))

(define (persistent-score-score score)
  (cdr score))

(define (save-score name)
  (let ((scores (reverse (get-high-scores)))
        (current (make-persistent-score name (score))))
    (call-with-output-file (list path: (resource "high-scores.txt"))
      (lambda (p)
        ;; Write all the scores back, adding the current score if
        ;; there is less than 10 scores, or replacing the lowest score
        ;; if the current score is higher
        (pp
         (if (< (length scores) 10)
             (cons current scores)
             (if (> (score) (persistent-score-score (car scores)))
                 (cons current (cdr scores))
                 scores))
         p)))))

(define (get-high-scores)
  (let ((path (resource "high-scores.txt")))
    (if (file-exists? path)
        (sort-list
         (let ((lst (with-input-from-file path read)))
           (if (pair? lst) lst '()))
         (lambda (el1 el2)
           (> (persistent-score-score el1)
              (persistent-score-score el2))))
        '())))

;; player

(define FAILED #f)
(define SIGNALLED #f)

(define (player-update)
  (define (end)
    (set! SIGNALLED #t))

  (if (not SIGNALLED)
      (cond
       ((life-is-dead?)
        (on-death)
        (end))
       
       ((player-failed?)
        (on-fail)
        (end)))))

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
  (reset-kill-count)
  (score-remove)
  (score-setup))
