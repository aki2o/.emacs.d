(bundle coffee-mode)
(use-package coffee-mode
  :defer t

  :init
  
  (mmask-regist-extension-with-icase 'coffee-mode "coffee" "iced")
  (mmask-regist-name 'coffee-mode "Cakefile")
  
  :config

  (add-hook 'coffee-mode-hook '~coffee-mode-setup t)
  (defun ~coffee-mode-setup ()
    (set (make-local-variable 'tab-width) 2)
    (set (make-local-variable 'coffee-tab-width) 2)
    ;; color-moccur
    (setq moccur-grep-default-mask (mmask-get-regexp-string 'coffee-mode)))
  )

