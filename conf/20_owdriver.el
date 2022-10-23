(bundle owdriver :type git :url "git@github.com:aki2o/owdriver.git" :branch "follow_up_pophint_update")
(use-package owdriver
  :config
  (add-to-list 'owdriver-keep-driving-command-prefixes "~scroll-" t)
  (add-to-list 'owdriver-keep-driving-command-prefixes "~beginning-of-" t)
  (add-to-list 'owdriver-keep-driving-command-prefixes "~end-of-" t)
  (add-to-list 'owdriver-keep-driving-command-prefixes "~find-" t)

  (add-to-list 'owdriver-keep-driving-commands '~pophint:forward t)
  (add-to-list 'owdriver-keep-driving-commands '~pophint:backward t)

  (owdriver-config-default)

  (owdriver-define-command ~scroll-down)
  (owdriver-define-command ~scroll-up)
  (owdriver-define-command ~imenu)

  (with-eval-after-load 'sequential-command
    (add-to-list 'owdriver-keep-driving-commands 'seq-beginning-of-line t)
    ;; yaol
    (add-to-list 'owdriver-keep-driving-commands 'seq-yaol-heads t)
    (add-to-list 'owdriver-keep-driving-commands 'seq-yaol-current-heads t))

  (with-eval-after-load 'pophint-autoloads
    (add-to-list 'owdriver-keep-driving-commands 'pophint:do-yaol-head t))

  (with-eval-after-load 'yaol
    (add-to-list 'owdriver-keep-driving-command-prefixes "yaol-" t))
  )

