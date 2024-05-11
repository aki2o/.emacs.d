(~add-setup-hook 'python-mode
  (when (functionp '~lsp-deferred)
    (~lsp-deferred)))

;; (use-package jedi
;;   :defer t)


;; (use-package jedi-direx
;;   :after (jedi))
