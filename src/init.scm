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

;; SLIME support

;; (expand-if SIMULATOR
;;            (include "/Users/james/projects/scheme/gambit/swank-gambit/swank-gambit.scm"))

(expand-if SIMULATOR
           (begin
             (include "../emacs/remote-debugger/debuggee.scm")
             (make-rdi-host "localhost:20000")

             (thread-start!
              (make-thread
               (lambda () (##repl-debug-main))))))

;; compile in all the ffis

(include "lib/resource.scm")
(include "ffi/ffi.scm")
(include "ffi/gl.scm")
(include "ffi/osx.scm")
(include "ffi/iphone.scm")
(include "ffi/image.scm")
(include "ffi/al.scm")
(include "ffi/ftgl.scm")
(include "ffi/view.scm")

;; load the game

(expand-if SIMULATOR
           (load (local-resource "src/farmageddon"))
           (include "farmageddon.scm"))

;; provide entry points

(c-define (c-init) () void "init" ""
  (init))

(c-define (c-render) () void "render" ""
  (render))
