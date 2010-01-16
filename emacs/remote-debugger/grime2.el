
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
         (master (= id 2)))

    (if master (grime-cleanup-processes))

    (save-current-buffer
      (message "making client...")
      (set-buffer (make-comint name "grime-client"
                               nil (number-to-string port)))
      (inferior-scheme-mode)
      (if master (grime-make-scheme-buffer)))))

(defun grime ()
  (interactive)
  (grime-kill)
  (let ((proc (start-process "grime"
                             "*grime-messages*"
                             "grime"
                             "20000")))
    (set-process-filter proc 'grime-receive-command)
    (message "Grime started...")))

(defun grime-make-scheme-buffer ()
  (interactive)
  (setq scheme-buffer (current-buffer)))

(defun grime-any-buffersp ()
  (setq res nil)
  (dolist (buf (buffer-list) res)
    (setq res
          (or res
              (string-match "grime-client" (buffer-name buf))))))

(defun grime-cleanup-processes ()
  (interactive)
  (dolist (buf (buffer-list))
    (if (string-match "grime-client" (buffer-name buf))
        (progn
          (if (get-buffer-process buf)
              (kill-process (get-buffer-process buf)))))))

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
