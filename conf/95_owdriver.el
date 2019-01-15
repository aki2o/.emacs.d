(bundle owdriver)
(use-package owdriver
  :config
  (unbind-key "C-o")
  (setq owdriver-prefix-key "C-o")
  (owdriver-config-default)

  (owdriver-add-keymap "M-h" 'owdriver-do-scroll-right)
  (owdriver-add-keymap "M-l" 'owdriver-do-scroll-left)

  
  (use-package pophint-config
    :defer t
    :init
    (owdriver-define-command pophint:do-flexibly t)
    (owdriver-define-command pophint:do t (pophint:do :not-switch-window t)))


  ;; (use-package counsel
  ;;   :defer t
  ;;   :init
  ;;   (owdriver-define-command counsel-imenu t))

  
  (use-package helm
    :defer t
    :init
    (owdriver-define-command helm-semantic-or-imenu t))


  (use-package sequential-command-config
    :defer t
    :init
    (owdriver-define-command seq-beginning-of-line t)
    (owdriver-define-command seq-recenter t))

  
  (use-package inertial-scroll
    :defer t)

  
  (owdriver-mode 1)
  )
