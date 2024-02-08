(use-package typescript-mode
  :defer t
  :custom ((typescript-indent-level 2))
  :init
  ;; (with-eval-after-load 'mmask
  ;;   (mmask-regist-extension-with-icase 'typescript-mode "ts")
  ;;   (mmask-regist-extension-with-icase 'typescript-mode "tsx"))

  (define-derived-mode typescript-tsx-mode typescript-mode "TSX")
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-tsx-mode))

  (with-eval-after-load 'tree-sitter
    (tree-sitter-require 'tsx)
    (add-to-list 'tree-sitter-major-mode-language-alist '(typescript-tsx-mode . tsx)))

  (with-eval-after-load 'flycheck
    (flycheck-add-mode 'javascript-eslint 'typescript-mode))

  :config
  (~add-setup-hook 'typescript-mode
    ;; 開くのが遅くなってそうなので、一旦やめ
    ;; (when (string-match-p "\\.tsx\\'" (buffer-name))
    ;;   (~run-deferred (current-buffer) (poly-tsx-mode)))
    (setq ~tidy-code-current-function '~typescript-tidy-dwim)
    ;; npm i -g typescript-language-server typescript が必要
    (when (functionp '~lsp-deferred)
      (~lsp-deferred)))

  (~add-setup-hook-after-load 'mmask 'typescript-mode
    (setq moccur-grep-default-mask (mmask-get-regexp-string 'typescript-mode)))

  (~add-setup-hook-after-load 'flycheck 'typescript-mode
    (~typescript-flycheck-select-dwim))

  (add-hook 'typescript-mode-hook 'eldoc-mode t)

  (define-hostmode poly-tsx-hostmode :mode 'typescript-tsx-mode)

  (define-innermode poly-tsx-gql-innermode
    :mode 'graphql-mode
    :head-matcher (rx (and (or "gql" "graphql") (? "(") "`"))
    :tail-matcher "`"
    :head-mode 'host
    :tail-mode 'host
    :fallback-mode 'host)

  (define-innermode poly-tsx-css-innermode
    :mode 'css-mode
    :head-matcher (rx (and (or "styled" "css") (? (+ (any "." "(" ")" "<" ">" alnum))) "`"))
    :tail-matcher "`"
    :head-mode 'host
    :tail-mode 'host
    :fallback-mode 'host)

  (define-polymode poly-tsx-mode
    :hostmode 'poly-tsx-hostmode
    :innermodes '(poly-tsx-gql-innermode poly-tsx-css-innermode))
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
