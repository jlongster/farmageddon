
(defun animattack-send-string (str)
  (comint-send-string (scheme-proc) str))

(defun animattack-restart ()
  (interactive)
  (animattack-send-string "(set! LIFE (current-max-life)")
  (animattack-send-string "(set! EXISTING-CRACKS '())")
  (animattack-send-string "(set! FAILED #f)")
  (animattack-send-string "(set! SCORE 0)")
  (animattack-send-string "(set-screen! level-screen)")
  (animattack-send-string "(load-randomized-cracks)")
  (animattack-send-string "\n"))

(global-set-key "\C-x\C-r" 'animattack-restart)



