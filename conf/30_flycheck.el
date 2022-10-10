(bundle flycheck)
(use-package flycheck
  :defer t
  :custom ((flycheck-check-syntax-automatically '(save mode-enabled))
           (flycheck-checker-error-threshold 50)))
