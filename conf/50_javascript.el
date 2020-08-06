;; eslint と連携させて実行するには
;; npm i -g prettier prettier-eslint eslint eslint-config-prettier eslint-plugin-prettier
(use-package prettier-js
  :defer t
  :commands (prettier-js))


(use-package add-node-modules-path
  :hook (js-mode js2-mode typescript-mode web-mode))


(use-package rjsx-mode
  :defer t
  :init
  (add-to-list 'auto-mode-alist '(".*\\.js\\'" . rjsx-mode))

  :config
  (add-hook 'rjsx-mode-hook '~rjsx-mode-setup t)
  (defun ~rjsx-mode-setup ()
    (setq indent-tabs-mode nil)
    (setq js-indent-level 2)
    (setq js2-strict-missing-semi-warning nil) ;;行末のセミコロンの警告はオフ
    (setq ~tidy-code-current-function '~typescript-tidy-dwim))
  )


(use-package js2-mode
  :defer t
  :init
  (mmask-regist-extension-with-icase 'js2-mode "js" "jse" "gs" "js.erb")

  :config
  (add-hook 'js2-mode-hook '~js2-mode-setup t)
  (defun ~js2-mode-setup ()
    (setq js-indent-level 2)
    (setq ~tidy-code-current-function '~typescript-tidy-dwim)
    ;; color-moccur
    (setq moccur-grep-default-mask (mmask-get-regexp-string 'js2-mode)))

  ;; flymake
  ;; (defun flymake-js16-init ()
  ;;   (let* ((temp-file (flymake-init-create-temp-buffer-copy 'flymake-create-temp-inplace))
  ;;          (local-file (file-relative-name temp-file (file-name-directory buffer-file-name))))
  ;;     (list "~/app/spidermonkey-1.6/js" (list "-sC" local-file))))
  ;; (defun flymake-js17-init ()
  ;;   (let* ((temp-file (flymake-init-create-temp-buffer-copy 'flymake-create-temp-inplace))
  ;;          (local-file (file-relative-name temp-file (file-name-directory buffer-file-name))))
  ;;     (list "js" (list "-sC" local-file))))

  ;; ;; .js書くときはJavaScript 1.6相当。user.js書くときは1.7相当でsyntaxチェック
  ;; (push (list (mmask-get-regexp-string 'js-mode) 'flymake-js16-init) flymake-allowed-file-name-masks)
  ;; (push '("/user\\.js\\'" flymake-js17-init) flymake-allowed-file-name-masks)

  ;; (defun flymake-js-load ()
  ;;   (interactive)
  ;;   (setq flymake-err-line-patterns '(("^\\(.+\\)\:\\([0-9]+\\)\: \\(SyntaxError\:.+\\)\:$" 1 2 nil 3)))
  ;;   (append flymake-err-line-patterns '(("^\\(.+\\)\:\\([0-9]+\\)\: \\(strict warning: trailing comma.+\\)\:$" 1 2 nil 3)))
  ;;   (local-set-key (kbd "C-b") 'flymake-goto-next-error-and-popup)
  ;;   (local-set-key (kbd "C-S-b") 'flymake-goto-prev-error-and-popup)
  ;;   (flymake-mode t))

  ;; (add-hook 'js2-mode-hook 'flymake-js-load t)
  )


(use-package json-mode
  :defer t
  :init
  (mmask-regist-extension-with-icase 'json-mode "json")
  (mmask-regist-name 'json-mode ".tern-project")

  :config
  (defun ~json-setup-mode ()
    (setq js-indent-level 2)
    (setq ~tidy-code-current-function '~typescript-tidy-dwim))
  (add-hook 'json-mode-hook '~json-setup-mode t))


(use-package tern
  :defer t
  :commands (tern-mode)
  :init
  (dolist (h '(rjsx-mode-hook js2-mode-hook nxml-mode-hook))
    (add-hook h '(lambda () (tern-mode 1)) t))

  :config
  (bind-keys :map tern-mode-keymap
             ("C-'"   . tern-get-docs)
             ("C->"   . tern-find-definition)
             ("M-."   . nil)
             ("M-RET" . tern-ac-complete))
  
  (~tags-be-find-tag-command tern-show-definition)

  ;; (add-to-list 'tern-command "--no-port-file" t)

  ;; (~tramp-use-original-buffer-file-name-in tern-get-type)
  ;; (~tramp-use-original-buffer-file-name-in tern-get-docs)
  ;; (~tramp-use-original-buffer-file-name-in tern-find-definition)
  ;; (~tramp-use-original-buffer-file-name-in tern-find-definition-by-name)
  ;; (~tramp-use-original-buffer-file-name-in tern-pop-find-definition)

  (when (fboundp 'pophint-tags:advice-command)
    (pophint-tags:advice-command tern-find-definition)))


;; エラーになってしまったのでコメントアウト
;; (use-package company-tern
;;   :after (tern))


;; (use-package tern-auto-complete
;;   :after (tern)
;;   :config
;;   (tern-ac-setup))

;; ;; For debug p-r

;; (eval-after-load 'tern-auto-complete
;;   '(progn

;; (defun tern-ac-complete-request (cc)
;;   (setq tern-last-point-pos (point))
;;   (setq tern-ac-complete-reply nil)
;;   (setq tern-ac-complete-request-point (point))
;;   (tern-run-query 
;;    (lambda (data) 
;;      (tern-ac-complete-response data)
;;      (funcall cc))
;;    `((type . "completions") (types . t) (docs . t) (caseInsensitive . t))
;;    (point)))

;; ;; (defun tern-ac-complete ()
;; ;;   "Complete code at point by tern."
;; ;;   (interactive)
;; ;;   (tern-ac-complete-request
;; ;;    (lambda ()
;; ;;      (let ((ac-sources ac-sources))
;; ;;        (add-to-list 'ac-sources 'ac-source-tern-completion)
;; ;;        (ac-start)))))

;; ))


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
    (setq ~tidy-code-current-function '~typescript-tidy-dwim)
    ;; color-moccur
    (setq moccur-grep-default-mask (mmask-get-regexp-string 'typescript-mode))))

(defun ~typescript-flycheck-select-dwim ()
  (cond ((projectile-file-exists-p (expand-file-name "tslint.json" (projectile-project-root)))
         (flycheck-select-checker 'typescript-tslint))
        ((executable-find "eslint")
         (flycheck-select-checker 'javascript-eslint))))

;; npm i -g @typescript-eslint/eslint-plugin が必要
(defun ~typescript-tidy-dwim ()
  (cond ((not (string= (shell-command-to-string "npm ls --parseable --depth 0 prettier | grep prettier") ""))
         (prettier-js))
        ((projectile-file-exists-p (expand-file-name "tslint.json" (projectile-project-root)))
         (~dockerize-shell-command (format "$(npm bin)/tslint --fix %s" (shell-quote-argument (~projectile-relative-path (current-buffer))))))
        (t
         (~dockerize-shell-command (format "$(npm bin)/eslint --fix %s" (shell-quote-argument (~projectile-relative-path (current-buffer))))))))


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
