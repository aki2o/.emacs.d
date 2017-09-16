;; (bundle bts-evernote)
(bundle bts-github)
(bundle date-field)
(use-package bts
  :defer t
  
  :config

  (use-package bts-github)

  ;; [WARN] trace level happens high memory consumption
  (bts--log-set-level 'debug)
  (bts--log-enable-logging)
  ;; (date-field--log-enable-logging)

  )

