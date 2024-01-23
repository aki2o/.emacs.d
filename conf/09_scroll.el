(use-package inertial-scroll
  :custom ((inertias-initial-velocity 60.0))
  :config
  (defalias 'scroll-up-command 'inertias-up)
  (defalias 'scroll-down-command 'inertias-down)

  (with-eval-after-load 'view
    (define-key view-mode-map [remap ~view-scroll-up] 'inertias-up)
    (define-key view-mode-map [remap ~view-scroll-down] 'inertias-down))

  ;; undefined function follow-mode とエラーになってしまうので、 require してみてる
  (require 'follow))
