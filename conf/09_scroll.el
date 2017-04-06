(bundle kiwanami/emacs-inertial-scroll :name inertial-scroll)
(use-package inertial-scroll
  :defer t

  :commands (inertias-up inertias-down)
  
  :init
  
  (setq inertias-initial-velocity 60.0)
  ;; (inertias-global-minor-mode 1)
  
  (global-set-key [remap scroll-up] 'inertias-up)
  (global-set-key [remap scroll-down] 'inertias-down)

  (define-key view-mode-map [remap ~view-scroll-up] 'inertias-up)
  (define-key view-mode-map [remap ~view-scroll-down] 'inertias-down)

  )

