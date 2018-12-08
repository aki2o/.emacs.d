(bundle flycheck)

(use-package flycheck
  :init
  (setq flycheck-check-syntax-automatically '(save mode-enabled)))
