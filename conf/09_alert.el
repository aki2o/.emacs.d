(bundle alert)
(bundle gntp)
(use-package alert
  :commands (alert)

  :init

  (setq alert-default-style
        (cond ((~is-mac) 'notifier)))
  )

