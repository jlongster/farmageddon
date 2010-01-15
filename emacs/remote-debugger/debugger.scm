#!/usr/bin/env gsi-script

;;; File: "debugger.scm"

(include "rdi.scm")

;;;----------------------------------------------------------------------------

(define console-window-num 0)

(define (new-console-window-num)
  (let ((x (+ console-window-num 1)))
    (set! console-window-num x)
    x))

(define (open-console-window console-id)
  (let ((tcp-port (+ 9000 (new-console-window-num))))
    (pp `(grime-open-client ,console-id ,tcp-port))
    ;; (let ((window
    ;;        (open-process
    ;;         (list path: "xterm"
    ;;               arguments: (list "-e"
    ;;                                "gsi"
    ;;                                "pump.scm"
    ;;                                (numbqer->string tcp-port)))))))
    (let loop ()
      (let ((port
             (with-exception-catcher
              (lambda (e)
                #f)
              (lambda ()
                (let ((port (open-tcp-client
                             (list server-address: "localhost"
                                   port-number: tcp-port))))
                  (tcp-client-peer-socket-info port)
                  port)))))
        (if (not port)
            (begin
              (thread-sleep! .1) ;; wait until the pump starts
              (loop))
            port)))))

;;;-----------------------------------------------------------------------------

(define rdi #f)

(define (rdi-function fn)
  (case fn
    ((register-console)
     rdi-register-console)
    ((console-output)
     rdi-console-output)
    (else
     (error "unknown function"))))

(define (main #!optional port)
  (set! rdi (rdi-create-server (and port (string->number port))))
  (rdi-force-connection rdi))

;;;-----------------------------------------------------------------------------

(define rdi-console-table (make-table))

(define (rdi-register-console console-id)
  (let ((console-port (open-console-window console-id)))
    (table-set! rdi-console-table console-id console-port)
    (rdi-console-input-pump-start! console-id console-port)
    #f))

(define (rdi-console-output console-id output)
  (let ((console-port
         (table-ref rdi-console-table console-id #f)))
    (if console-port
        (begin
          (display output console-port)
          (force-output console-port))))
  #t)

(define (read-substring-blocking-for-1 str start end port)
  (if (< start end)
      (begin
        (input-port-timeout-set! port +inf.0) ;; block for the first byte
        (let ((n (read-substring str start (+ start 1) port)))
          (input-port-timeout-set! port -inf.0) ;; don't block for the rest
          (if (= n 1)
              (+ 1 (read-substring str (+ start 1) end port))
              n)))
      0))

(define (rdi-console-input-pump-start! console-id console-port)
  (thread-start!
   (make-thread
    (lambda ()
      (let* ((buflen 1000)
             (buf (make-string buflen)))
        (let loop ()
          (let ((n (read-substring-blocking-for-1 buf 0 buflen console-port)))
            (if (> n 0)
                (begin
                  (rdi-remote-call rdi
                                   'console-input
                                   console-id
                                   (substring buf 0 n))
                  (loop))))))))))

;;;-----------------------------------------------------------------------------
