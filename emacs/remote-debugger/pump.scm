#!

;;; File: "pump.scm"

;; start like this:
;;
;;  % xterm gsi -:tE pump.scm 9000
;;
;; then
;;
;;  % telnet localhost 9000

(define port-num (string->number (cadr (command-line))))

(define listen-port (open-tcp-server port-num))

(define connection (read listen-port))

(define (read-subu8vector-blocking-for-1 u8vect start end port)
  (if (< start end)
      (begin
        (input-port-timeout-set! port +inf.0) ;; block for the first byte
        (let ((n (read-subu8vector u8vect start (+ start 1) port)))
          (input-port-timeout-set! port -inf.0) ;; don't block for the rest
          (if (= n 1)
              (+ 1 (read-subu8vector u8vect (+ start 1) end port))
              n)))
      0))

(define (copy in out)
  (let* ((buflen 1000)
         (buf (make-u8vector buflen)))
    (let loop ()
      (let ((n (read-subu8vector-blocking-for-1 buf 0 buflen in)))
        (if (> n 0)
            (begin
              (write-subu8vector buf 0 n out)
              (force-output out)
              (loop)))))))

(let ((in (repl-input-port))
      (out (repl-output-port)))
  (if (tty? in)
      (tty-mode-set! in #f #t #f #f 38400)) ;; disallow ^C
  (thread-start! (make-thread (lambda ()
                                (copy connection out)
                                (exit))))
  (copy in connection))
