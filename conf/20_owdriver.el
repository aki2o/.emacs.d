(bundle owdriver :type git :url "git@github.com:aki2o/owdriver.git" :branch "follow_up_pophint_update")
(use-package owdriver
  :custom ((owdriver-next-window-function '~owdriver-next-window))
  :config
  (add-to-list 'owdriver-keep-driving-command-prefixes "~scroll-" t)
  (add-to-list 'owdriver-keep-driving-command-prefixes "~beginning-of-" t)
  (add-to-list 'owdriver-keep-driving-command-prefixes "~end-of-" t)
  (add-to-list 'owdriver-keep-driving-command-prefixes "~find-" t)

  (add-to-list 'owdriver-keep-driving-commands '~pophint:forward t)
  (add-to-list 'owdriver-keep-driving-commands '~pophint:backward t)

  (owdriver-config-default)

  (owdriver-add-keymap [remap 'delete-other-windows] 'owdriver-quit)
  (owdriver-add-keymap [remap 'delete-window] 'owdriver-focus-window)

  (owdriver-define-command ~scroll-down)
  (owdriver-define-command ~scroll-up)
  (owdriver-define-command ~imenu)
  (owdriver-define-command isearch-forward)

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

(defun ~owdriver-next-window (reverse)
  (let ((w (and (functionp 'e2wm:managed-p)
                (e2wm:managed-p)
                (eq (e2wm:$pst-name (e2wm:pst-get-instance)) 'transcribe)
                (wlf:get-window (e2wm:pst-get-wm) 'right))))
    (if (and (window-live-p w)
             (not (eq w owdriver--window)))
        w
      (owdriver-find-next-window reverse))))

