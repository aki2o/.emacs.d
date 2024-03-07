(use-package flycheck
  :defer t
  :custom ((flycheck-check-syntax-automatically '(save mode-enabled))
           (flycheck-checker-error-threshold 50))
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode))
