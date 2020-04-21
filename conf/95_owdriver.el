(bundle owdriver)
(use-package owdriver
  :config
  (unbind-key "C-o")
  (setq owdriver-prefix-key "C-o")
  (owdriver-config-default)

  (with-eval-after-load 'pophint
    (owdriver-define-command pophint:do-flexibly t)
    (owdriver-define-command pophint:do t (pophint:do :not-switch-window t))
    (owdriver-define-command pophint:do-outline-heading nil))

  (with-eval-after-load 'helm
    (owdriver-define-command helm-semantic-or-imenu t))

  (with-eval-after-load 'sequential-command-config
    (owdriver-define-command seq-beginning-of-line t)
    (owdriver-define-command seq-recenter t)
    ;; yaol
    (owdriver-define-command seq-yaol-heads         nil)
    (owdriver-define-command seq-yaol-current-heads nil))

  (with-eval-after-load 'yaol
    (owdriver-define-command yaol-fold-clear-current    nil)
    (owdriver-define-command yaol-fold-clear-all        nil)
    (owdriver-define-command yaol-up-head               nil)
    (owdriver-define-command yaol-next-sibling-head     nil)
    (owdriver-define-command yaol-previous-sibling-head nil)
    (owdriver-define-command yaol-down-head             nil))

  (owdriver-mode 1))
