(add-to-list '~browse-internal-url-list "https://docs.rs/")
(add-to-list '~browse-internal-url-list "https://doc.rust-lang.org")

(use-package rust-mode
  :defer t
  :custom ((rust-format-on-save t)
           (rust-indent-offset 2))
  :config
  (~add-setup-hook 'rust-mode
    (setq lsp-rust-server 'rust-analyzer)
    (setq lsp-rust-analyzer-server-command '("rust-analyzer"))
    (when (functionp '~lsp-deferred)
      (~lsp-deferred))))


(use-package cargo
  :defer t
  :hook (rust-mode . cargo-minor-mode))
