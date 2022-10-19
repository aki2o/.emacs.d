;; eslint と連携させて実行するには
;; npm i -g prettier prettier-eslint eslint eslint-config-prettier eslint-plugin-prettier
(bundle prettier-js)
(use-package prettier-js
  :defer t
  :commands (prettier-js))


(bundle add-node-modules-path)
(use-package add-node-modules-path
  :hook (js-mode js2-mode typescript-mode web-mode))


(bundle rjsx-mode)
(use-package rjsx-mode
  :defer t
  ;; :init
  ;; (add-to-list 'auto-mode-alist '(".*\\.js\\'" . rjsx-mode))
  :config
  (add-hook 'rjsx-mode-hook '~rjsx-mode-setup t))

(defun ~rjsx-mode-setup ()
  (setq indent-tabs-mode nil)
  (setq js-indent-level 2)
  (setq js2-strict-missing-semi-warning nil) ;;行末のセミコロンの警告はオフ
  (setq ~tidy-code-current-function '~typescript-tidy-dwim))


(bundle js2-mode)
(use-package js2-mode
  :defer t
  :init
  (with-eval-after-load 'mmask
    (mmask-regist-extension-with-icase 'js2-mode "js" "jse" "gs" "js.erb"))
  :config
  (add-hook 'js2-mode-hook '~js2-mode-setup t))

(defun ~js2-mode-setup ()
  (setq js-indent-level 2)
  (setq ~tidy-code-current-function '~typescript-tidy-dwim)
  (when (find-library-name "mmask")
    (setq moccur-grep-default-mask (mmask-get-regexp-string 'js2-mode))))


(bundle json-mode)
(use-package json-mode
  :defer t
  :init
  (with-eval-after-load 'mmask
    (mmask-regist-extension-with-icase 'json-mode "json")
    (mmask-regist-name 'json-mode ".tern-project"))
  :config
  (defun ~json-setup-mode ()
    (setq js-indent-level 2)
    (setq ~tidy-code-current-function '~typescript-tidy-dwim))
  (add-hook 'json-mode-hook '~json-setup-mode t))


(bundle typescript-mode)
(use-package typescript-mode
  :defer t
  :custom ((typescript-indent-level 2))
  :init
  (with-eval-after-load 'mmask
    (mmask-regist-extension-with-icase 'typescript-mode "ts"))

  (with-eval-after-load 'flycheck
    (flycheck-add-mode 'javascript-eslint 'typescript-mode))

  :config
  (add-hook 'typescript-mode-hook '~typescript-mode-setup t)
  (add-hook 'typescript-mode-hook 'eldoc-mode t)
  )

(defun ~typescript-mode-setup ()
  (when (find-library-name "projectile")
    (~typescript-flycheck-select-dwim)
    (setq ~tidy-code-current-function '~typescript-tidy-dwim))

  (when (find-library-name "mmask")
    (setq moccur-grep-default-mask (mmask-get-regexp-string 'typescript-mode))))

(defun ~typescript-flycheck-select-dwim ()
  (cond ((projectile-file-exists-p (expand-file-name "tslint.json" (projectile-project-root)))
         (flycheck-select-checker 'typescript-tslint))
        ((executable-find "eslint")
         (flycheck-select-checker 'javascript-eslint))))

;; npm i -g @typescript-eslint/eslint-plugin が必要
(defun ~typescript-tidy-dwim ()
  (interactive)
  (cond ((not (string= (shell-command-to-string "npm ls --parseable --depth 0 prettier | grep prettier") ""))
         (prettier-js))
        ((projectile-file-exists-p (expand-file-name "tslint.json" (projectile-project-root)))
         (~dockerize-shell-command (format "$(npm bin)/tslint --fix %s" (shell-quote-argument (~projectile-relative-path (current-buffer))))))
        (t
         (~dockerize-shell-command (format "$(npm bin)/eslint --fix %s" (shell-quote-argument (~projectile-relative-path (current-buffer))))))))


(bundle tide)
(use-package tide
  :defer t
  :custom ((tide-format-options '(:indentSize 2 :tabSize 2)))
  :hook (typescript-mode . ~tide-mode-setup))

(defun ~tide-mode-setup ()
  (tide-setup)
  (tide-hl-identifier-mode +1)
  (company-mode +1)
  (setq ~find-definition-function 'tide-jump-to-definition)
  (setq ~find-references-function 'tide-references)
  ;; (add-hook 'before-save-hook 'tide-format-before-save)
  )


(bundle web-mode)
(use-package web-mode
  :defer t
  :custom ((web-mode-code-indent-offset 2)
           (web-mode-markup-indent-offset 2)
           (web-mode-attr-indent-offset 2)
           (web-mode-css-indent-offset 2))
  :init
  (with-eval-after-load 'mmask
    (mmask-regist-extension-with-icase 'web-mode "tsx"))

  (with-eval-after-load 'flycheck
    (flycheck-add-mode 'javascript-eslint 'web-mode)
    (flycheck-add-mode 'typescript-tslint 'web-mode))

  :config
  (add-hook 'web-mode-hook '~tsx-setup t))

(defun ~tsx-setup ()
  (when (string-equal "tsx" (file-name-extension buffer-file-name))
    (setq ~tidy-code-current-function '~typescript-tidy-dwim)
    (~tide-mode-setup)
    (~typescript-flycheck-select-dwim)))

