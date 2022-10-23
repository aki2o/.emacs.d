(bundle rust-mode)
(use-package rust-mode
  :defer t
  :custom ((rust-format-on-save t)
           (rust-indent-offset 2))
  :config
  (~add-setup-hook-after-load 'lsp-mode 'rust-mode
    (setq lsp-rust-server 'rust-analyzer)
    (setq lsp-rust-analyzer-server-command '("rust-analyzer"))
    (lsp-deferred)))


(bundle cargo)
(use-package cargo
  :defer t
  :hook (rust-mode . cargo-minor-mode))

