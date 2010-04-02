;;;; "init"
;;; Pulls together all of the modules for our system and provides a C
;;; interface for the outside world.

(include "lib/macros.scm")
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

;; provide entry points

(c-define (c-init) () void "init" ""
  (init))

(c-define (c-render) () void "render" ""
  (render))
