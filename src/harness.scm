
(declare (block)
         (standard-bindings)
         (extended-bindings))

(include "lib/macros.scm")
(include "config.scm")
(include "lib/srfi/srfi-1.scm")
(include "lib/srfi/srfi-2.scm")
(include "lib/srfi/sort.scm")
(include "lib/events#.scm")
(include "lib/events.scm")

;; load the game

(expand-if SIMULATOR
           (load (local-resource "src/farmageddon")))
