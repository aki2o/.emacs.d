(use-package typescript-mode
  :defer t
  :init
  (mmask-regist-extension-with-icase 'typescript-mode "ts")
  (setq typescript-indent-level 2)

  (with-eval-after-load 'flycheck
    (flycheck-add-mode 'javascript-eslint 'typescript-mode))

  :config
  (add-hook 'typescript-mode-hook '~typescript-mode-setup t)
  (defun ~typescript-mode-setup ()
    (~typescript-flycheck-select-dwim)
    (setq ~tidy-code-current-function 'prettier-js) ;; npm i -g @typescript-eslint/eslint-plugin が必要
    ;; color-moccur
    (setq moccur-grep-default-mask (mmask-get-regexp-string 'typescript-mode))))

(defun ~typescript-flycheck-select-dwim ()
  (cond ((projectile-file-exists-p (expand-file-name "tslint.json" (projectile-project-root)))
         (flycheck-select-checker 'typescript-tslint))
        ((executable-find "eslint")
         (flycheck-select-checker 'javascript-eslint))))


(use-package tide
  :defer t
  :hook (typescript-mode . ~tide-mode-setup)
  :init
  (setq tide-format-options '(:indentSize 2 :tabSize 2))
  
  (defun ~tide-mode-setup ()
    (tide-setup)
    (flycheck-mode +1)
    (eldoc-mode +1)
    (tide-hl-identifier-mode +1)
    (company-mode +1)
    ;; (add-hook 'before-save-hook 'tide-format-before-save)
    ))


(use-package web-mode
  :defer t
  :init
  (mmask-regist-extension-with-icase 'web-mode "tsx")

  (with-eval-after-load 'flycheck
    (flycheck-add-mode 'javascript-eslint 'web-mode)
    (flycheck-add-mode 'typescript-tslint 'web-mode))

  :config
  (add-hook 'web-mode-hook '~tsx-setup t)
  (defun ~tsx-setup ()
    (when (string-equal "tsx" (file-name-extension buffer-file-name))
      (setq web-mode-code-indent-offset 2)
      (setq web-mode-markup-indent-offset 2)
      (setq web-mode-attr-indent-offset 2)
      (setq web-mode-css-indent-offset 2)
      (setq ~tidy-code-current-function 'prettier-js) ;; npm i -g @typescript-eslint/eslint-plugin が必要
      (~tide-mode-setup)
      (~typescript-flycheck-select-dwim))))
