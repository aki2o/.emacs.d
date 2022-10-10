(bundle alert)
(use-package alert
  :defer t
  :commands (alert)
  :init
  (setq alert-default-style
        (cond ((~is-mac) 'notifier))))
