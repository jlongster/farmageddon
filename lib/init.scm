;;;; "init"
;;; Pulls together all of the modules for our system and provides a C
;;; interface for the outside world.

(define-macro (expand-eval expr)
  (eval expr))

(define-macro (define-expand-var name value)
  `(expand-eval (define ,name ,value)))

(define-macro (expand-if cond then . else)
  `(expand-eval (if ,cond
                    ',then
                    ',(if (null? else) (void) (car else)))))

(include "config.scm")

;; debugger

(expand-if SIMULATOR
           (begin
             (include "util/remote-debugger/debuggee.scm")
             (rdi-set-host! "localhost:20000")

             (thread-start!
              (make-thread
               (lambda () (##repl-debug-main))))))

;; compile in all the ffis

(include "resource.scm")
(include "ffi/ffi.scm")
(include "ffi/gl.scm")
(include "ffi/gl-util.scm")
(include "ffi/osx.scm")
(include "ffi/iphone.scm")
(include "ffi/image.scm")
(include "ffi/al.scm")

;; load the game

(expand-if SIMULATOR
           (load (local-resource "lib/animattack"))
           (include "animattack.scm"))

;; provide entry points

(c-define (c-init) () void "init" ""
  (init))

(c-define (c-render) () void "render" ""
  (render))

(c-define (c-get-title) () char-string "get_title" ""
  (get-title))
