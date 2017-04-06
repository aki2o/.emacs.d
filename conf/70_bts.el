;; (bundle bts-evernote)
(bundle bts-github)
(bundle date-field)
(use-package bts

  :bind* (("M-b n"   . bts:ticket-new)
          ("M-b s"   . bts:summary-open)
          ("M-b p n" . bts:project-new)
          ("M-b p u" . bts:project-update)
          ("M-b p d" . bts:project-remove)
          ("M-b p D" . bts:project-remove-all)
          ("M-b q n" . bts:query-new)
          ("M-b q u" . bts:query-update)
          ("M-b q d" . bts:query-remove)
          ("M-b q D" . bts:query-remove-all))

  :init
  
  (unbind-key "M-b")

  :config

  (use-package bts-github)
  (use-package bts-evernote)
  ;; (require 'bts-org)

  ;; [WARN] trace level happens high memory consumption
  (bts--log-set-level 'debug)
  (bts--log-enable-logging)
  ;; (date-field--log-enable-logging)

  )

