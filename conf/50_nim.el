(use-package nim-mode
  :defer t
  :hook (nim-mode . ~nim-setup))

(defun ~nim-setup ()
  (when (fboundp 'lsp) (lsp)))
