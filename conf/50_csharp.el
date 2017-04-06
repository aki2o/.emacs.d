(bundle emacswiki:csharp-mode)
(use-package csharp-mode
  :defer t

  :init
  
  (mmask-regist-extension-with-icase 'csharp-mode "cs")

  :config

  (add-hook 'csharp-mode-hook '~csharp-mode-setup t)
  (defun ~csharp-mode-setup ()
    (setq comment-column 40)
    (setq c-basic-offset 4)
    (font-lock-add-magic-number)
    (c-set-offset 'substatement-open 0)
    (c-set-offset 'case-label '+)
    (c-set-offset 'arglist-intro '+)
    (c-set-offset 'arglist-close 0)
    ;; color-moccur
    (setq moccur-grep-default-mask (mmask-get-regexp-string 'csharp-mode)))

  )

