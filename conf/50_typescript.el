;; typescript-mode
;; http://www.typescriptlang.org/ からダウンロード
;; ファイル名をtypescript.elに変更。動作しない箇所、hookの定義がない対応のため内容も変更
(bundle tide)
;; (bundle tss)
(use-package typescript-mode
  :defer t
  
  :init
  
  (mmask-regist-extension-with-icase 'typescript-mode "ts")

  :config

  (add-hook 'typescript-mode-hook '~typescript-mode-setup t)
  (defun ~typescript-mode-setup ()
    ;; color-moccur
    (setq moccur-grep-default-mask (mmask-get-regexp-string 'typescript-mode)))

  (use-package tide
    :config

    (setq tide-format-options '(:indentSize 2 :tabSize 2))
    
    (defun ~tide-mode-setup ()
      (tide-setup)
      (flycheck-mode +1)
      (eldoc-mode +1)
      (tide-hl-identifier-mode +1)
      ;; company is an optional dependency. You have to
      ;; install it separately via package-install
      ;; `M-x package-install [ret] company`
      (company-mode +1))
    (add-hook 'typescript-mode-hook '~tide-mode-setup t)

    ;; (add-hook 'before-save-hook 'tide-format-before-save)
    )
  
  ;; (use-package tss
  ;;   :config
    
  ;;   (setq tss-popup-help-key "C-'")
  ;;   (setq tss-jump-to-definition-key "C->")
  ;;   (setq tss-implement-definition-key "C-c i")
  ;;   (tss-config-default)
    
  ;;   (use-package pophint-config
  ;;     :defer t
  ;;     :config
  ;;     (pophint-config:set-tag-jump-command tss-jump-to-definition)))

  )

