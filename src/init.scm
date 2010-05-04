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
(include "ffi/feint.scm")

;; provide entry points

(c-define (c-save-score) () void "save_score" ""
  (save-score))

(c-define (c-on-scores-loaded arr) (NSArray*) void "on_scores_loaded" ""
  (on-scores-loaded
   (and arr
        (map
         (lambda (score-obj)
           (make-persistent-score
            (HighScore-name score-obj)
            (HighScore-score score-obj)))
         (NSArray->list arr)))))

(c-define (c-load-global-scores) () void "load_global_scores" ""
  (if (eq? (current-screen) scores-screen)
      (begin
        (set! *have-scores?* #f)
        (load-global-scores))))

(c-define (c-init) () void "init" ""
  (init))

(c-define (c-render) () void "render" ""
  (render))
