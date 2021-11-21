(use-package rust-mode
  :defer t
  :custom ((rust-format-on-save t)
           (rust-indent-offset 2))
  :hook (rust-mode . ~rust-setup))

(defun ~rust-setup ()
  (when (fboundp 'lsp)
    (setq lsp-rust-server 'rust-analyzer)
    (setq lsp-rust-analyzer-server-command '("rust-analyzer"))
    (lsp)))

(use-package cargo
  :defer t
  :hook (rust-mode . cargo-minor-mode))
