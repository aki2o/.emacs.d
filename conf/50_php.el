(use-package php-mode
  :defer t
  :init
  (dolist (re '("\\.php[s345t]?\\'" "\\.phtml\\'" "\\.inc\\'"))
    (add-to-list 'auto-mode-alist `(,re . php-mode) t))

  :config
  (add-hook 'php-mode-hook '~php-mode-setup t)

  (defun ~php-mode-setup ()
    ;; auto-complete.el
    (add-to-list 'ac-sources 'ac-source-yasnippet)
    (local-set-key (kbd ">") 'self-insert-with-ac-trigger-command)))


(use-package php-completion
  :after (php-mode)
  :config
  (defun ~php-cmp-setup ()
    (php-completion-mode 1)
    (add-to-list 'ac-sources 'ac-source-php-completion))
  (add-hook 'php-mode-hook '~php-cmp-setup t))


(use-package php-eldoc
  :after (php-mode)
  :config
  (defun ~php-eldoc-setup ()
    (php-eldoc-enable)
    ;; (cond
    ;;   ((string-match-p "^/my-project-folder")
    ;;    (php-eldoc-probe-load "http://my-project.com/probe.php?secret=sesame"))
    ;;   ((string-match-p "^/other-project-folder")
    ;;    (php-eldoc-probe-load "http://localhost/otherproject/probe.php?secret=sesame")))
    )
  (add-hook 'php-mode-hook '~php-eldoc-setup t))
