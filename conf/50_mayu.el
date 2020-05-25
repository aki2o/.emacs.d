(use-package mayu-mode
  :straight (:host github :repo "byplayer/yamy")
  :if (~is-windows)
  :defer t
  :init
  (mmask-regist-extension-with-icase 'mayu-mode "mayu")

  :config
  (add-hook 'mayu-mode-hook '~mayu-mode-setup t)
  
  (defun ~mayu-mode-setup ()
    (add-hook 'after-save-hook '~mayu-create-helpinfo t)
    (add-to-list 'align-rules-list '(mayu-key-value (regexp . "=") (modes . '(mayu-mode))))
    (add-to-list 'align-rules-list '(mayu-comment (regexp . "#") (modes . '(mayu-mode))))
    (when (fboundp 'mmask-get-regexp-string)
      (setq moccur-grep-default-mask (mmask-get-regexp-string 'mayu-mode))))

  (defun ~mayu-create-helpinfo ()
    (interactive)
    (when (string= (file-name-extension buffer-file-name) "mayu")
      (start-process-shell-command "mayu-create-helpinfo" nil (concat "mkmayuhelp " (shell-quote-argument default-directory))))))
