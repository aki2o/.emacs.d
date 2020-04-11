(bundle owdriver)
(use-package owdriver
  :config
  (unbind-key "C-o")
  (setq owdriver-prefix-key "C-o")
  (owdriver-config-default)

  ;; Outline
  (owdriver-define-command seq-outline-headings            nil)
  (owdriver-define-command seq-outline-current-headings    nil)
  (owdriver-define-command ~outline-narrow-current-heading nil)
  (owdriver-define-command outline-show-all                nil)
  (owdriver-define-command outline-up-heading              nil)
  (owdriver-define-command outline-forward-same-level      nil)
  (owdriver-define-command outline-backward-same-level     nil)
  (owdriver-define-command outline-next-visible-heading    nil)

  (with-eval-after-load 'pophint
    (owdriver-define-command pophint:do-flexibly t)
    (owdriver-define-command pophint:do t (pophint:do :not-switch-window t))
    (owdriver-define-command pophint:do-outline-heading nil))

  (with-eval-after-load 'helm
    (owdriver-define-command helm-semantic-or-imenu t))

  (with-eval-after-load 'sequential-command-config
    (owdriver-define-command seq-beginning-of-line t)
    (owdriver-define-command seq-recenter t))
  
  (owdriver-mode 1))
