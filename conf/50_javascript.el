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
    (setq my:lint-executable "$(npm bin)/eslint --fix")))


(use-package js2-mode
  :defer t
  :init
  (with-eval-after-load 'mmask
    (mmask-regist-extension-with-icase 'js2-mode "js" "jse" "gs" "js.erb"))
  :config
  (~add-setup-hook 'js2-mode
    (setq js-indent-level 2)
    (setq my:lint-executable "$(npm bin)/eslint --fix"))

  (~add-setup-hook-after-load 'mmask 'js2-mode
    (setq moccur-grep-default-mask (mmask-get-regexp-string 'js2-mode)))
  )
