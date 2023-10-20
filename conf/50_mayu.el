(bundle byplayer/yamy :name mayu-mode)
(use-package mayu-mode
  :defer t
  :init
  (with-eval-after-load 'mmask
    (mmask-regist-extension-with-icase 'mayu-mode "mayu"))

  :config
  (~add-setup-hook 'mayu-mode
    (add-hook 'after-save-hook '~mayu-create-helpinfo t t)

    (add-to-list 'align-rules-list '(mayu-key-value (regexp . "=") (modes . '(mayu-mode))))
    (add-to-list 'align-rules-list '(mayu-comment (regexp . "#") (modes . '(mayu-mode)))))

  (~add-setup-hook-after-load 'mmask 'mayu-mode
    (setq moccur-grep-default-mask (mmask-get-regexp-string 'mayu-mode))))

(defun ~mayu-create-helpinfo ()
  "Create infomation of bind and help from config of mayu."
  (interactive)
  (when (string= (file-name-extension buffer-file-name) "mayu")
    (start-process-shell-command "mayu-create-helpinfo" nil (concat "mkmayuhelp " (shell-quote-argument default-directory)))))
