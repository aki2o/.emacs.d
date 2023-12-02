(use-package go-mode
  :defer t
  :config
  (~add-setup-hook 'go-mode
    (setq-local tab-width 2)

    (when (functionp 'lsp)
      (add-hook 'before-save-hook 'lsp-format-buffer t t)
      (add-hook 'before-save-hook 'lsp-organize-imports t t)
      (lsp-deferred)))

  (~add-setup-hook-after-load 'mmask 'go-mode
    (setq moccur-grep-default-mask (mmask-get-regexp-string 'go-mode)))

  (with-eval-after-load 'lsp-mode
    (lsp-register-custom-settings '(("gopls.completeUnimported" t t))))
  )


(use-package protobuf-mode
  :defer t)


(use-package gom-mode
  :defer t)
