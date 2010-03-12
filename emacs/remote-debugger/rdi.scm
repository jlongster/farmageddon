;;; File: "rdi.scm"

;;;----------------------------------------------------------------------------

(define default-remote-debugger-address "192.168.1.162")
(define default-remote-debugger-port-num 20000)

(define (rdi-set-host! address)
  (set! default-remote-debugger-address address))

(define (split-address str)
  (call-with-input-string
      str
    (lambda (port)
      (let* ((x (read-all port (lambda (port) (read-line port #\:))))
             (len (length x)))
        (cond ((<= len 1)
               (cons (if (= len 1)
                         (car x)
                         default-remote-debugger-address)
                     default-remote-debugger-port-num))
              ((= len 2)
               (let ((address (car x))
                     (port-num (string->number (cadr x) 10)))
                 (if (and port-num
                          (exact? port-num)
                          (integer? port-num)
                          (>= port-num 1)
                          (<= port-num 65535))
                     (cons (if (string=? address "")
                               default-remote-debugger-address
                               address)
                           port-num)
                     #f)))
              (else
               #f))))))

;;;----------------------------------------------------------------------------

(define-type rdi
  address
  port-num
  seq-num
  call-table
  connection
  writer-thread
)

(define (rdi-create-client remote-debugger-address)
  (and remote-debugger-address
       (let ((x (split-address remote-debugger-address)))
         (if (not x)
             (error "invalid remote debugger address")
             (let* ((address
                     (car x))
                    (port-num
                     (cdr x))
                    (rdi
                     (make-rdi
                      address
                      port-num
                      0
                      (make-table)
                      #f
                      #f))
                    (writer-thread
                     (rdi-create-writer-thread rdi)))
               (rdi-writer-thread-set! rdi writer-thread)
               (thread-start! writer-thread)
               rdi)))))

(define (rdi-create-server remote-debugger-port-num)
  (let* ((address
          #f)
         (port-num
          (or remote-debugger-port-num
              default-remote-debugger-port-num))
         (rdi
          (make-rdi
           address
           port-num
           0
           (make-table)
           #f
           #f))
         (writer-thread
          (rdi-create-writer-thread rdi)))
    (rdi-writer-thread-set! rdi writer-thread)
    (thread-start! writer-thread)
    rdi))

(define (rdi-force-connection rdi)
  (or (rdi-connection rdi)
      (if (rdi-address rdi)
          (rdi-open-client rdi)
          (rdi-open-server rdi))))

(define rdi-version1 '());(gambit-debuggee-version 0))
(define rdi-version2 '());(gambit-debugger-version 0))

(define (rdi-open-client rdi)
  (let ((connection
         (open-tcp-client
          (list server-address: (rdi-address rdi)
                port-number: (rdi-port-num rdi)))))

    (write rdi-version1 connection)
    (force-output connection)

    (let ((response (read connection)))
      (if (not (equal? response rdi-version2))
          (error "unexpected debugger version")
          (let ((reader-thread (rdi-create-reader-thread rdi connection)))
            (rdi-connection-set! rdi connection)
            (thread-start! reader-thread)
            connection)))))

(define (rdi-open-server rdi)
  (let ((listen-port
         (open-tcp-server
          (list server-address: (string-append "*:" (number->string (rdi-port-num rdi)))
                reuse-address: #t))))
    (let loop ()
      (let ((connection
             (read listen-port)))
        (let ((request (read connection)))
          (if (not (equal? request rdi-version1))
              (error "unexpected debuggee version")
              (begin

                (write rdi-version2 connection)
                (force-output connection)

                (let ((reader-thread (rdi-create-reader-thread rdi connection)))
                  (rdi-connection-set! rdi connection)
                  (thread-start! reader-thread)
                  (loop)))))))))

(define (rdi-create-reader-thread rdi connection)
  (make-thread
   (lambda ()
     (let loop ()
       (let ((msg (read connection)))
         (if (not (eof-object? msg))
             (begin
               (thread-send (rdi-writer-thread rdi) msg)
               (loop)))))
     (thread-send (rdi-writer-thread rdi) '(reader-thread-terminated)))))

(define (rdi-create-writer-thread rdi)
  (make-thread
   (lambda ()
     (let loop ()
       (let ((msg (thread-receive)))
         (and (rdi-handle-message rdi msg)
              (loop)))))))

(define (rdi-new-seqnum rdi)
  (let ((seq-num (+ (rdi-seq-num rdi) 1)))
    (rdi-seq-num-set! rdi seq-num)
    seq-num))

(define (rdi-handle-message rdi msg)
  (if (pair? msg)

      (case (car msg)

        ((reader-thread-terminated)
         ;; (pretty-print
         ;;  '(rdi reader-thread is terminating)
         ;;  ##stdout-port)
         #t)

        ((terminate)
         ;; (pretty-print
         ;;  '(rdi writer-thread is terminating)
         ;;  ##stdout-port)
         #f)

        ((call)
         (let* ((seq-num
                 (cadr msg))
                (call
                 (caddr msg))
                (result
                 (apply (rdi-function (car call))
                        (cdr call))))
           (rdi-send rdi (list 'return seq-num result))
           #t))

        ((remote-call)
         (let* ((result-mutex (cadr msg))
                (call (caddr msg))
                (seq-num (rdi-new-seqnum rdi)))
           (rdi-send rdi (list 'call seq-num call))
           (table-set! (rdi-call-table rdi)
                       seq-num
                       result-mutex)
           #t))

        ((return)
         (let* ((seq-num (cadr msg))
                (result (caddr msg))
                (call-table (rdi-call-table rdi))
                (result-mutex (table-ref call-table seq-num #f)))
           (if (not result-mutex)
               (error "invalid call sequence number")
               (begin
                 (table-set! call-table seq-num)
                 (mutex-specific-set! result-mutex result)
                 (mutex-unlock! result-mutex)
                 #t))))

        (else
         (pretty-print
          (list 'unhandled-message msg)
          ##stdout-port)
         #t))

      #f))

(define (rdi-send rdi msg)
  (let ((connection (rdi-connection rdi)))
    (write msg connection)
    (force-output connection)))

(define (rdi-remote-call rdi fn . args)
  (rdi-force-connection rdi)
  (let ((result-mutex (make-mutex 'remote-call)))
    (mutex-lock! result-mutex) ;; result not ready yet...
    (thread-send
     (rdi-writer-thread rdi)
     (list 'remote-call result-mutex (cons fn args)))
    (mutex-lock! result-mutex) ;; wait until result is ready
    (mutex-specific result-mutex)))

;;;-----------------------------------------------------------------------------
