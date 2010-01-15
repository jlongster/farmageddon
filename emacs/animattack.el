
(defun animattack-send-string (str)
  (comint-send-string (scheme-proc) str))

(defun animattack-restart ()
  (interactive)
  (animattack-send-string "(set! %%life %%max-life)")
  (animattack-send-string "(set! %%cracks '())")
  (animattack-send-string "(set! %%crack-points '())")
  (animattack-send-string "(set! %%crack-dirty #t)")
  (animattack-send-string "\n"))

(global-set-key "\C-x\C-r" 'animattack-restart)



