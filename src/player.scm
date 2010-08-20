;;;; player
;;; Keeps track of player's statistics like life and score

(declare (block)
         (standard-bindings)
         (extended-bindings))

(include "lib/macros.scm")
(include "config.scm")

;; life

(define LIFE #f)
(define VICTOR #f)

(define (get-damage el) 1)

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
  (set! SCORE 0)
  (set! *current-pers-score* #f))

(define (reset-kill-count)
  (set! KILL-COUNT 0))

(define (make-persistent-score name score)
  (cons name score))

(define (persistent-score-name score)
  (car score))

(define (persistent-score-score score)
  (cdr score))

(define (persistent-score-name-set! score name)
  (set-car! score name))

(define *current-pers-score* #f)

(define (save-score-with-name name)
  ;; If we have already saved the score, we are just changing the name
  (if *current-pers-score*
      (persistent-score-name-set! *current-pers-score* name)
      (let ((scores (get-high-scores))
            (current (make-persistent-score name (score))))
        ;; Inject it into the current high scores
        (set! *high-scores* (%inject-score scores current))
        ;; Submit score to feint
        (expand-if (not LITE)
                   (submit-high-score (score)))
        (set! *current-pers-score* current)))

  (call-with-output-file (writable "high-scores.txt")
    (lambda (p)
      (write *high-scores* p))))

(define (%inject-score scores n)
  (let ((scores (%sort-scores (cons n scores))))
    (if (< (length scores) 8)
        scores
        (take scores 8))))

(define (save-score)
  (save-score-with-name
   (let ((name (high-score-field-value)))
     (if (equal? name "") "Soldier" name))))

(define *high-scores* #f)

(define (get-high-scores)
  (if *high-scores*
      *high-scores*
      (let ((scores (%get-high-scores)))
        (set! *high-scores* scores)
        scores)))

(define (get-highest-score)
  (let ((scores (get-high-scores)))
    (if (pair? scores)
        (persistent-score-score (car scores))
        0)))

(define (%get-high-scores)
  (let ((path (writable "high-scores.txt")))
    (if (file-exists? path)
        (%sort-scores
         (let ((lst (with-input-from-file path read)))
           (if (pair? lst) lst '())))
        '())))

(define (%sort-scores scores)
  (sort-list scores
             (lambda (el1 el2)
               (> (persistent-score-score el1)
                  (persistent-score-score el2)))))

;; instructions

(define (player-seen-instructions?)
  (let ((path (writable "instructions.txt")))
    (and (file-exists? path)
         (with-input-from-file path read))))

(define (player-saw-instructions)
  (call-with-output-file (writable "instructions.txt")
    (lambda (p)
      (write #t))))

;; config

(define (save-sound)
  (call-with-output-file (writable "settings.txt")
    (lambda (p)
      (write (list 'sound (if (is-audio-muted?) 'off 'on)) p))))

(define (read-sound)
  (let ((path (writable "settings.txt")))
    (if (file-exists? path)
        (let* ((config (with-input-from-file path read))
               (key (cadr config)))
          (if (eq? key 'on) #t #f))
        #t)))

;; player

(define FAILED #f)
(define SIGNALLED #f)
(define LITE-SCORE 16000)

(define (player-update)
  (define (end)
    (set! SIGNALLED #t))

  (if (not SIGNALLED)
      (cond
       ((expand-if LITE (> (score) LITE-SCORE) #f)
        (set! FAILED #t)
        (on-trial-ended)
        (end))
       
       ((life-is-dead?)
        (on-death)
        (end))
       
       ((player-failed?)
        (on-fail)
        (end)))))

(define (player-heal)
  (background-pop (make-vec4d 0. 1. 0. 1.))
  (set! LIFE (min (current-max-life)
                  (+ LIFE 5)))
  (let loop ((i 0))
    (if (and (< i 5)
             (not (null? EXISTING-CRACKS)))
        (begin
          (set! PRELOADED-CRACKS
                (cons (car (car EXISTING-CRACKS))
                      PRELOADED-CRACKS))
          (set! EXISTING-CRACKS (cdr EXISTING-CRACKS))
          (loop (+ i 1))))))

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
