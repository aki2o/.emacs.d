;; eslint と連携させて実行するには
;; npm i -g prettier prettier-eslint eslint eslint-config-prettier eslint-plugin-prettier
(use-package prettier-js
  :defer t
  :commands (prettier-js))


(use-package add-node-modules-path
  :defer t
  :init
  (dolist (h '(js-mode-hook js2-mode-hook typescript-mode-hook web-mode-hook))
    (add-hook h 'add-node-modules-path t)))


(use-package rjsx-mode
  :defer t
  ;; :init
  ;; (add-to-list 'auto-mode-alist '(".*\\.js\\'" . rjsx-mode))
  :config
  (~add-setup-hook 'rjsx-mode
    (setq indent-tabs-mode nil)
    (setq js-indent-level 2)
    (setq js2-strict-missing-semi-warning nil) ;;行末のセミコロンの警告はオフ
    (setq ~tidy-code-current-function '~typescript-tidy-dwim)))


(use-package js2-mode
  :defer t
  :init
  (with-eval-after-load 'mmask
    (mmask-regist-extension-with-icase 'js2-mode "js" "jse" "gs" "js.erb"))
  :config
  (~add-setup-hook 'js2-mode
    (setq js-indent-level 2)
    (setq ~tidy-code-current-function '~typescript-tidy-dwim))

  (~add-setup-hook-after-load 'mmask 'js2-mode
    (setq moccur-grep-default-mask (mmask-get-regexp-string 'js2-mode)))
  )


(use-package json-mode
  :defer t
  :init
  (with-eval-after-load 'mmask
    (mmask-regist-extension-with-icase 'json-mode "json")
    (mmask-regist-name 'json-mode ".tern-project"))
  :config
  (~add-setup-hook 'json-mode
    (setq-local js-indent-level 2)
    (setq ~tidy-code-current-function '~typescript-tidy-dwim)))


(use-package typescript-mode
  :defer t
  :custom ((typescript-indent-level 2))
  :init
  (with-eval-after-load 'mmask
    (mmask-regist-extension-with-icase 'typescript-mode "ts")
    (mmask-regist-extension-with-icase 'typescript-mode "tsx"))

  (with-eval-after-load 'flycheck
    (flycheck-add-mode 'javascript-eslint 'typescript-mode))

  :config
  (~add-setup-hook 'typescript-mode
    (when (string-match-p "\\.tsx\\'" (buffer-name))
      (~run-deferred (current-buffer) (poly-tsx-mode)))
    (setq ~tidy-code-current-function '~typescript-tidy-dwim)
    ;; npm i -g typescript-language-server typescript が必要
    (when (functionp '~lsp-deferred)
      (~lsp-deferred)))

  (~add-setup-hook-after-load 'mmask 'typescript-mode
    (setq moccur-grep-default-mask (mmask-get-regexp-string 'typescript-mode)))

  (~add-setup-hook-after-load 'flycheck 'typescript-mode
    (~typescript-flycheck-select-dwim))

  (add-hook 'typescript-mode-hook 'eldoc-mode t)

  (define-hostmode poly-ts-hostmode :mode 'typescript-mode)

  (define-innermode poly-ts-gql-innermode
    :mode 'graphql-mode
    :head-matcher (rx (and (or "gql" "graphql") "`"))
    :tail-matcher "`"
    :head-mode 'host
    :tail-mode 'host
    :fallback-mode 'host)

  (define-innermode poly-ts-css-innermode
    :mode 'css-mode
    :head-matcher (rx (and (or "styled" "css") (? (+ (any "." "(" ")" "<" ">" alnum))) "`"))
    :tail-matcher "`"
    :head-mode 'host
    :tail-mode 'host
    :fallback-mode 'host)

  (define-polymode poly-tsx-mode
    :hostmode 'poly-ts-hostmode
    :innermodes '(poly-ts-gql-innermode poly-ts-css-innermode))
  )

(defun ~typescript-flycheck-select-dwim ()
  (cond ((and (functionp 'projectile-project-root)
              (projectile-file-exists-p (expand-file-name "tslint.json" (projectile-project-root))))
         (flycheck-select-checker 'typescript-tslint))
        ((executable-find "eslint")
         (flycheck-select-checker 'javascript-eslint))))

;; npm i -g @typescript-eslint/eslint-plugin が必要
(defun ~typescript-tidy-dwim ()
  (interactive)
  (cond ((not (string= (shell-command-to-string "npm ls --parseable --depth 0 prettier | grep prettier") ""))
         (prettier-js))
        ((and (functionp 'projectile-project-root)
              (projectile-file-exists-p (expand-file-name "tslint.json" (projectile-project-root))))
         (~dockerize-shell-command (format "$(npm bin)/tslint --fix %s" (shell-quote-argument (~projectile-relative-path (current-buffer))))))
        ((functionp '~projectile-relative-path)
         (~dockerize-shell-command (format "$(npm bin)/eslint --fix %s" (shell-quote-argument (~projectile-relative-path (current-buffer))))))
        (t
         (error "Can't ~typescript-tidy-dwim"))))
