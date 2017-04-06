(bundle markdown-mode)
(use-package markdown-mode
  :defer t
  :config
  (bind-keys :map markdown-mode-map
             ("C-c C-c" . ~markdown-render-buffer))
  )


(defun ~markdown-render-buffer ()
  (interactive)
  (let* ((tmpfile (~tmpfile "grip.html"))
         (cmd (format "grip --gfm --export '%s' '%s'" (expand-file-name (buffer-file-name)) tmpfile))
         (url (format "file:///%s" (replace-regexp-in-string
                                    "\\`/" "" (replace-regexp-in-string "\\\\" "/" tmpfile)))))
    (shell-command cmd nil nil)
    (~browse-url url t)))

