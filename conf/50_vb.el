(use-package visual-basic-mode
  :straight nil
  :if (~is-windows)
  :defer t
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
  
  (when (fboundp 'ap:add-project)
    (ap:add-project :name 'vba :look-for '(".vba-project"))))


(use-package vbasense
  :after (visual-basic-mode)
  :config
  (setq vbasense-popup-help-key "C-'")
  (setq vbasense-jump-to-definition-key "C->")
  (add-to-list 'vbasense-tli-files "c:/Program Files/Common Files/System/ado/msado21.tlb")
  (add-to-list 'vbasense-tli-files "c:/Program Files/Common Files/Microsoft Shared/DAO/dao360.dll")
  (setq vbasense-setup-user-library-function
        '(lambda ()
           (vbasense-load-directory "x:/MyLib/vba")
           (vbasense-load-project)))
  (vbasense-config-default)
  
  (when (fboundp 'pophint-tags:advice-command)
    (pophint-tags:advice-command vbasense-jump-to-definition)))
