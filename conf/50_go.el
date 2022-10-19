(bundle go-mode)
(use-package go-mode
  :defer t
  :config
  (add-hook 'go-mode-hook '~go-setup-mode t)

  (with-eval-after-load 'lsp
    (lsp-register-custom-settings '(("gopls.completeUnimported" t t))))
  )

(defun ~go-setup-mode ()
  (setq-local tab-width 2)

  (when (find-library-name "mmask")
    (setq moccur-grep-default-mask (mmask-get-regexp-string 'go-mode)))

  (when (find-library-name "lsp-mode")
    (add-hook 'before-save-hook 'lsp-format-buffer t t)
    (add-hook 'before-save-hook 'lsp-organize-imports t t)
    (lsp-deferred)))


(bundle protobuf-mode)
(use-package protobuf-mode
  :defer t)


(bundle gom-mode)
(use-package gom-mode
  :defer t)

