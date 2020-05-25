(defadvice kill-region (before clipboard-cut activate)
  (when (eq last-command this-command)
    (set-register clipboard-register (car kill-ring))
    (message "Copy to register")))

(defadvice kill-ring-save (before clipboard-copy activate)
  (when (eq last-command this-command)
    (set-register clipboard-register (car kill-ring))
    (message "Copy to register")))

(defun ~register-paste ()
  (interactive)
  (insert-register clipboard-register)
  (message "Paste from register"))
