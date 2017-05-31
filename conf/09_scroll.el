(bundle kiwanami/emacs-inertial-scroll :name inertial-scroll)
(use-package inertial-scroll
  :defer t

  :commands (inertias-up inertias-down)
  
  :init
  
  (setq inertias-initial-velocity 60.0)
  ;; (inertias-global-minor-mode 1)
  
  (define-key view-mode-map [remap ~view-scroll-up] 'inertias-up)
  (define-key view-mode-map [remap ~view-scroll-down] 'inertias-down)

  )

