
Commands:

(##thread-heartbeat-interval-set! (exact->inexact 1/1000))
(profile-start!)
(profile-stop!)
(write-profile-report "/tmp/farmageddon/" "192.168.1.162:7777")

(##interrupt-vector-set!
 1
 (let ((count 0)
       (time (real-time)))
   (lambda ()
     (##thread-heartbeat!)
     (set! count (+ count 1))

     (let ((diff (- (real-time) time)))
       (if (> diff 1.)
           (begin
             (pp (list diff count))
             (set! count 0)
             (set! time (real-time))))))))
