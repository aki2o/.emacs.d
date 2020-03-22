;; typescript-mode
;; http://www.typescriptlang.org/ からダウンロード
;; ファイル名をtypescript.elに変更。動作しない箇所、hookの定義がない対応のため内容も変更
(use-package typescript-mode
  :defer t
  
  :init
  (mmask-regist-extension-with-icase 'typescript-mode "ts")
  (setq typescript-indent-level 2)

  :config
  (add-hook 'typescript-mode-hook '~typescript-mode-setup t)
  (add-hook 'typescript-mode-hook '~tide-mode-setup t)
  (defun ~typescript-mode-setup ()
    ;; color-moccur
    (setq moccur-grep-default-mask (mmask-get-regexp-string 'typescript-mode)))
  )


(bundle tide)
(use-package tide
  :commands (~tide-mode-setup)
  
  :init
  (setq tide-format-options '(:indentSize 2 :tabSize 2))
  
  (defun ~tide-mode-setup ()
    (tide-setup)
    (flycheck-mode +1)
    (eldoc-mode +1)
    (tide-hl-identifier-mode +1)
    (company-mode +1))

  :config
  ;; (add-hook 'before-save-hook 'tide-format-before-save)
  )


;; (bundle tss)
;; (use-package tss
;;   :config

;;   (setq tss-popup-help-key "C-'")
;;   (setq tss-jump-to-definition-key "C->")
;;   (setq tss-implement-definition-key "C-c i")
;;   (tss-config-default)

;;   (use-package pophint
;;     :defer t
;;     :config
;;     (pophint-tags:advice-command tss-jump-to-definition)))


(bundle web-mode)
(use-package web-mode
  :init
  (mmask-regist-extension-with-icase 'web-mode "tsx")
  
  :config
  (add-hook 'web-mode-hook '~tsx-setup t)
  (defun ~tsx-setup ()
    (when (string-equal "tsx" (file-name-extension buffer-file-name))
      (setq web-mode-code-indent-offset 2)
      (setq web-mode-markup-indent-offset 2)
      (setq web-mode-attr-indent-offset 2)
      (setq web-mode-css-indent-offset 2)
      (~tide-mode-setup)))

  (flycheck-add-mode 'typescript-tslint 'web-mode)
  )
