(bundle php-mode)
(use-package php-mode
  :defer t
  :init
  (with-eval-after-load 'mmask
    (mmask-regist-name-regexp 'php-mode ".*\\.php[s345t]?\\'")
    (mmask-regist-extension-with-icase 'php-mode "phtml" "inc")))
  

(bundle php-eldoc)
(use-package php-eldoc
  :after (php-mode)
  :config
  (~add-setup-hook-for-load 'php-eldoc 'php-mode
    (php-eldoc-enable)
    ;; (cond
    ;;   ((string-match-p "^/my-project-folder")
    ;;    (php-eldoc-probe-load "http://my-project.com/probe.php?secret=sesame"))
    ;;   ((string-match-p "^/other-project-folder")
    ;;    (php-eldoc-probe-load "http://localhost/otherproject/probe.php?secret=sesame")))
    ))
