(use-package json-mode
  :defer t
  :init
  (with-eval-after-load 'mmask
    (mmask-regist-extension-with-icase 'json-mode "json")
    (mmask-regist-name 'json-mode ".tern-project"))
  :config
  (~add-setup-hook 'json-mode
    (setq-local js-indent-level 2)
    (setq my:lint-executable (my:js-resolve-lint-executable))
    (tsi-json-mode 1)))
