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

  ;; 必要なさそうなので一旦コメントアウト
  ;; ;; https://github.com/emacs-lsp/lsp-mode/issues/3317#issuecomment-1026511720
  ;; (with-eval-after-load 'lsp-mode
  ;;   (lsp-make-interactive-code-action organize-imports-my-ts "source.organizeImports.ts"))

  :config
  (~add-setup-hook 'typescript-mode
    (setq my:lint-executable (my:js-resolve-lint-executable))
    (when (string-match-p "\\.tsx\\'" (buffer-name))
      (~run-deferred (current-buffer) 5 (poly-tsx-mode 1)))
    (~run-deferred (current-buffer) 5 (tsi-typescript-mode 1))
    ;; npm i -g typescript-language-server typescript が必要
    (when (functionp '~lsp-deferred)
      ;; (setq ~lsp-organize-imports-function 'lsp-organize-imports-my-ts)
      ;; 便利そうだけど現状困ってなくて最新の状況がわかってないのでコメントアウトしてる
      ;; https://emacs-lsp.github.io/lsp-mode/page/lsp-eslint/
      ;; (setq-local lsp-enabled-clients '(ts-ls eslint))
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

  (my:import-js-run))

(defun ~typescript-flycheck-select-dwim ()
  (cond ((and (functionp 'projectile-project-root)
              (projectile-file-exists-p (expand-file-name "tslint.json" (projectile-project-root))))
         (flycheck-select-checker 'typescript-tslint))
        ((executable-find "eslint")
         (flycheck-select-checker 'javascript-eslint))))
