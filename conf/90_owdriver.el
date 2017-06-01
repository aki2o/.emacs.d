(bundle owdriver)
(use-package owdriver
  :config
  
  (unbind-key "C-o")
  (setq owdriver-prefix-key "C-o")
  (owdriver-config-default)

  (owdriver-add-keymap "M-h" 'owdriver-do-scroll-right)
  (owdriver-add-keymap "M-l" 'owdriver-do-scroll-left)

  (use-package pophint
    :defer t
    :init
    (owdriver-define-command pophint:do-flexibly t))
  
  ;; (use-package counsel
  ;;   :defer t
  ;;   :init
  ;;   (owdriver-define-command counsel-imenu t))
  
  (use-package helm
    :defer t
    :init
    (owdriver-define-command helm-semantic-or-imenu t))
  
  (when (commandp 'seq-beginning-of-line)
    (owdriver-define-command seq-beginning-of-line t))
  
  (when (commandp 'seq-recenter)
    (owdriver-define-command seq-recenter t))
  
  (use-package inertial-scroll
    :defer t)
  
  (owdriver-mode 1)

  )

