(bundle emacswiki:visual-basic-mode)
(bundle vbasense)
(use-package visual-basic-mode

  :if (~is-windows)

  :commands (visual-basic-mode)
  
  :init
  
  (mmask-regist-extension-with-icase 'visual-basic-mode "vb" "vbs" "cls" "bas" "frm" "rvb")
  (setq visual-basic-mode-indent 4)
  ;; (setq visual-basic-ide-pathname "H:/VS98/VB98/VB6.EXE")

  :config

  (add-hook 'visual-basic-mode-hook '~visual-basic-mode-setup t)
  (defun ~visual-basic-mode-setup ()
    (add-to-list 'ac-sources 'ac-source-yasnippet)
    (setq moccur-grep-default-mask (mmask-get-regexp-string 'visual-basic-mode)))
  
  (use-package anything-project
    :config
    (ap:add-project :name 'vba :look-for '(".vba-project")))

  (use-package vbasense
    :config
    (setq vbasense-popup-help-key "C-'")
    (setq vbasense-jump-to-definition-key "C->")
    (add-to-list 'vbasense-tli-files "c:/Program Files/Common Files/System/ado/msado21.tlb")
    (add-to-list 'vbasense-tli-files "c:/Program Files/Common Files/Microsoft Shared/DAO/dao360.dll")
    (setq vbasense-setup-user-library-function
          '(lambda ()
             (vbasense-load-directory "x:/MyLib/vba")
             (vbasense-load-project)))
    (vbasense-config-default))
  
  (use-package pophint
    :defer t
    :config
    (pophint-tags:advice-command vbasense-jump-to-definition))

  )

