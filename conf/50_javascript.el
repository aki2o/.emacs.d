(defun my:js-resolve-lint-executable ()
  (let* ((command (cond ((projectile-file-exists-p (expand-file-name "pnpm-lock.yaml" (projectile-project-root)))
                         "pnpm")
                        ((projectile-file-exists-p (expand-file-name "yarn.lock" (projectile-project-root)))
                         "yarn")
                        (t
                         "npm")))
         (sub (cond ((projectile-file-exists-p (expand-file-name "tslint.json" (projectile-project-root)))
                     "tslint")
                    (t
                     "eslint"))))
    (format "%s exec %s --fix" command sub)))

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
    (setq my:lint-executable (my:js-resolve-lint-executable))))


(use-package js2-mode
  :defer t
  :init
  (with-eval-after-load 'mmask
    (mmask-regist-extension-with-icase 'js2-mode "js" "jse" "gs" "js.erb"))
  :config
  (~add-setup-hook 'js2-mode
    (setq js-indent-level 2)
    (setq my:lint-executable (my:js-resolve-lint-executable)))

  (~add-setup-hook-after-load 'mmask 'js2-mode
    (setq moccur-grep-default-mask (mmask-get-regexp-string 'js2-mode)))

  (my:import-js-run))


;; npm i -g import-js が必要
(use-package import-js
  :defer t
  :init
  (autoload 'import-js-check-daemon "import-js"))

(defun my:import-js-run ()
  (when (catch 'import-js-daemon
          (import-js-check-daemon)
          nil)
    (run-import-js)))
