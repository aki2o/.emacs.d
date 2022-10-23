(bundle coffee-mode)
(use-package coffee-mode
  :defer t
  :init
  (with-eval-after-load 'mmask
    (mmask-regist-extension-with-icase 'coffee-mode "coffee" "iced")
    (mmask-regist-name 'coffee-mode "Cakefile"))

  :config
  (~add-setup-hook 'coffee-mode
    (set (make-local-variable 'tab-width) 2)
    (set (make-local-variable 'coffee-tab-width) 2))

  (~add-setup-hook-after-load 'mmask 'coffee-mode
    (setq moccur-grep-default-mask (mmask-get-regexp-string 'coffee-mode))))

