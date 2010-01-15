
(defun get-grime-process ()
  (get-process "grime"))

(defun grime-receive-command (proc string)
  (message string)
  (grime-display-msg string)
  (grime-run-command string))

(defun grime-display-msg (str)
  (with-current-buffer (process-buffer (get-grime-process))
    (let ((move (= (point) (process-mark proc))))
      (save-excursion
        (goto-char (process-mark proc))
        (insert str)
        (set-marker (process-mark proc) (point)))
      (if move (goto-char (process-mark proc))))))

(defun grime-run-command (cmd)
  ;; Todo, don't do this
  (eval (read cmd)))

(defun grime-client-buffer-name (id)
  (concat "grime-client-" (number-to-string id)))

(defun grime-open-client (id port)
  (let* ((name (grime-client-buffer-name id))
         (buffer-name (concat "*" name "*"))
         (master (if (or (get-buffer buffer-name)
                         (not (grime-any-buffersp)))
                     (progn
                       (grime-cleanup)
                       t)
                   nil)))
    (set-buffer (make-comint name "grime-client"
                             nil (number-to-string port)))
    (inferior-scheme-mode)
    (condition-case err
        (split-window)
      (error (delete-other-windows) (split-window)))
    (display-buffer buffer-name)

    (if master (grime-make-scheme-buffer))))

(defun grime ()
  (interactive)
  (grime-kill)
  (let ((proc (start-process "grime"
                             "*grime-messages*"
                             "grime"
                             "20000")))
    (set-process-filter proc 'grime-receive-command)
    (with-current-buffer "*grime-messages*"
      (switch-to-buffer (current-buffer))
      (insert "\n\nFinding some grime...\n\n")
      (set-marker (process-mark proc) (point))
      (sit-for 1))
    (switch-to-buffer (current-buffer))))

(defun grime-make-scheme-buffer ()
  (interactive)
  (setq scheme-buffer (current-buffer)))

(defun grime-any-buffersp ()
  (setq res nil)
  (dolist (buf (buffer-list) res)
    (setq res
          (or res
              (string-match "grime-client" (buffer-name buf))))))

(defun grime-cleanup ()
  (interactive)
  (dolist (buf (buffer-list))
    (if (string-match "grime-client" (buffer-name buf))
        (progn
          (if (get-buffer-process buf)
              (kill-process (get-buffer-process buf)))
          (kill-buffer buf)
          (message "killing buffer...")))))

(defun grime-kill ()
  (interactive)
  (grime-cleanup)
  (let ((proc (get-process "grime")))
    (if proc (kill-process proc)))
  (let ((buf (get-buffer "*grime-messages*")))
    (if buf (kill-buffer buf))))
