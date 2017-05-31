(bundle owdriver)
(use-package owdriver
  :config
  
  (unbind-key "C-o")
  (setq owdriver-prefix-key "C-o")
  (owdriver-config-default)

  (owdriver-add-keymap "M-h" 'owdriver-do-scroll-right)
  (owdriver-add-keymap "M-l" 'owdriver-do-scroll-left)

  (global-set-key (kbd "M-h")   'owdriver-do-scroll-right)
  (global-set-key (kbd "M-j")   'owdriver-do-scroll-up)
  (global-set-key (kbd "M-k")   'owdriver-do-scroll-down)
  (global-set-key (kbd "M-l")   'owdriver-do-scroll-left)
  
  (use-package pophint
    :defer t
    :init
    (owdriver-define-command pophint:do-flexibly t))
  
  ;; (use-package counsel
  ;;   :defer t
  ;;   :init
  ;;   (owdriver-define-command counsel-imenu t)
  ;;   (global-set-key (kbd "H-i") 'owdriver-do-counsel-imenu))
  
  (use-package helm
    :defer t
    :init
    (owdriver-define-command helm-semantic-or-imenu t)
    (global-set-key (kbd "H-i") 'owdriver-do-helm-semantic-or-imenu))
  
  (when (commandp 'seq-beginning-of-line)
    (owdriver-define-command seq-beginning-of-line t))
  
  (when (commandp 'seq-recenter)
    (owdriver-define-command seq-recenter t))
  
  (use-package inertial-scroll
    :defer t
    :init
    (global-set-key [remap owdriver-do-scroll-up] 'owdriver-do-inertias-up)
    (global-set-key [remap owdriver-do-scroll-down] 'owdriver-do-inertias-down))
  
  (owdriver-mode 1)

  )

