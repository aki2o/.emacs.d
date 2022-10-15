(bundle recentf-ext)
(use-package recentf-ext)

(recentf-mode t)
(setq recentf-max-saved-items 3000)
(setq recentf-exclude '("/TAGS" "/ETAGS" "/tmp/"))

(defun ~recentf-push-buffers-in-frame ()
  (walk-windows
   (lambda (win)
     (let ((bfn (buffer-local-value 'buffer-file-name (window-buffer win))))
       (and bfn (recentf-add-file bfn))))))

(add-to-list 'window-configuration-change-hook '~recentf-push-buffers-in-frame)

(defun ~recentf-add-dired-directory ()
  (when (and (stringp dired-directory)
             (equal "" (file-name-nondirectory dired-directory)))
    (recentf-add-file dired-directory)))

(add-hook 'dired-mode-hook '~recentf-add-dired-directory)
