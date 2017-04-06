(bundle info+)
(use-package info
  :defer t

  :config

  (add-to-list 'Info-default-directory-list (expand-file-name (concat user-emacs-directory "info")))
  (add-to-list 'Info-mode-hook '~info-mode-setup t)
  (defun ~info-mode-setup ()
    ;; Infoバッファはview-modeにする
    (view-mode 1))

  (use-package info+)

  )

