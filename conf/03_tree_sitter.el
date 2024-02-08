;; Emacs29からはこっちを使うべきっぽいが、現状は tree-sitter の方が良さそう
;; (use-package treesit-auto
;;   :custom ((treesit-auto-install 'prompt)
;;            (treesit-font-lock-level 4))
;;   :config
;;   (treesit-auto-add-to-auto-mode-alist 'all)
;;   (global-treesit-auto-mode))


;; https://emacs-tree-sitter.github.io/syntax-highlighting/customization/
(use-package tree-sitter
  :init
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :after tree-sitter)
