(bundle go-mode)
(bundle go-rename)
(bundle go-eldoc)
(bundle go-projectile)
(bundle go-direx)
(bundle go-autocomplete)
(bundle company-go)
(use-package go-mode
  :init

  (setq gofmt-command "goimports") ; go get golang.org/x/tools/cmd/goimports

  :config

  (defun ~go-setup-mode ()
    (add-hook 'before-save-hook 'gofmt-before-save t t))
  
  (add-hook 'go-mode-hook '~go-setup-mode t)

  (defun ~godoc ()
    (interactive)
    (godoc (completing-read "Query: " (go-packages))))
  
  (define-key go-mode-map (kbd "C-'") '~godoc)
  (define-key go-mode-map (kbd "C->") 'godef-jump-other-window)

  (use-package pophint-config
    :config
    (pophint-config:set-tag-jump-command godef-jump-other-window))

  (use-package color-moccur
    :init
    (defun ~go-setup-color-moccur ()
      (setq moccur-grep-default-mask (mmask-get-regexp-string 'go-mode)))
    (add-hook 'go-mode-hook '~go-setup-color-moccur t))
  
  (use-package go-autocomplete
    :init
    (setq ac-go-expand-arguments-into-snippets nil))

  (use-package go-eldoc
    :init
    (add-hook 'go-mode-hook 'go-eldoc-setup t))

  (use-package go-direx
    :init
    (define-key go-mode-map (kbd "C-c C-j") 'go-direx-pop-to-buffer))

  (use-package go-projectile
    :config

    (defun go-projectile-set-local-keys ()))
  
  )


(bundle gom-mode)
(use-package gom-mode
  :defer t)

