(bundle corfu)
(use-package corfu
  :custom ((corfu-auto t)
           (corfu-auto-deley 0.4)
           (corfu-auto-prefix 4))

  :init
  (global-corfu-mode)

  :config
  (bind-keys :map corfu-map
             ("C-S-j" . corfu-next)
             ("C-S-k" . corfu-previous)
             ("C-z"   . corfu-quit))
  )


(bundle corfu-doc)
(use-package corfu-doc
  :after (corfu)
  :hook (corfu-mode-hook . corfu-doc-mode))


(bundle cape :type github :pkgname "minad/cape")
(use-package cape
  :hook (find-file-hook . ~cape-setup)
  :init
  (add-to-list 'completion-at-point-functions 'cape-keyword t))


(bundle kind-icon)
(use-package kind-icon
  :after (corfu)
  :custom ((kind-icon-default-face 'corfu-default))
  :config
  (add-to-list 'corfu-margin-formatters 'kind-icon-margin-formatter))

