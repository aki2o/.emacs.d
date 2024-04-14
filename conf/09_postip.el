(use-package pos-tip
  :defer t
  :init
  (setq pos-tip-foreground-color "white")
  (setq pos-tip-background-color "black"))


(use-package bufloat
  :defer t
  :config
  (bind-keys :map bufloat-frame-mode-map
             ("C-c <up>" . bufloat-cancel)))
