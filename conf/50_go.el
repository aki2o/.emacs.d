(use-package go-mode
  :defer t
  :config
  (add-hook 'go-mode-hook '~go-setup-mode t)

  (defun ~go-setup-mode ()
    (setq-local tab-width 2)
    (when (fboundp 'mmask-get-regexp-string)
      (setq moccur-grep-default-mask (mmask-get-regexp-string 'go-mode)))
    (when (fboundp 'lsp)
      (add-hook 'before-save-hook 'lsp-format-buffer t t)
      (add-hook 'before-save-hook 'lsp-organize-imports t t)
      (lsp-deferred)))

  (with-eval-after-load 'lsp
    (lsp-register-custom-settings
     '(("gopls.completeUnimported" t t))))

  )

(use-package gom-mode
  :defer t)
