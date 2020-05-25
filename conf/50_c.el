(use-package cc-mode
  :defer t
  :init
  (add-hook 'c-mode-common-hook '~cc-mode-setup t)
  
  :config
  (defun ~cc-mode-setup ()
    (c-set-style "k&r")
    (c-set-offset 'substatement-open 0)
    (c-set-offset 'case-label '+)
    (c-set-offset 'arglist-cont-nonempty '+)
    (c-set-offset 'arglist-intro '+)
    (c-set-offset 'topmost-intro-cont '+)
    (c-set-offset 'arglist-close 0)
    (setq tab-width 4)
    (setq c-basic-offset tab-width)
    (setq indent-tabs-mode nil)
    (setq c-auto-newline t)
    (setq c-tab-always-indent t)
    (when (fboundp mmask-get-regexp-string)
      (setq moccur-grep-default-mask (mmask-get-regexp-string 'cc-mode)))
    (when (fboundp flycheck-mode)
      (flycheck-mode 1))))


(use-package c-eldoc
  :defer t
  :init
  (add-hook 'c-mode-common-hook '~c-eldoc-setup t)
  (defun ~c-eldoc-setup ()
    (set (make-local-variable 'eldoc-idle-delay) 0.20)
    (c-turn-on-eldoc-mode)))
