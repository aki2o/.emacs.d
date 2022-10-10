(bundle inertial-scroll :type github :pkgname "kiwanami/emacs-inertial-scroll")
(use-package inertial-scroll
  :defer t
  :commands (inertias-up inertias-down)
  :init
  (setq inertias-initial-velocity 60.0)
  ;; (inertias-global-minor-mode 1)

  (with-eval-after-load 'view
    (define-key view-mode-map [remap ~view-scroll-up] 'inertias-up)
    (define-key view-mode-map [remap ~view-scroll-down] 'inertias-down))

  :config
  ;; undefined function follow-mode とエラーになってしまうので、 require してみてる
  (require 'follow))

