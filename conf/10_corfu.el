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
             ("C-z"   . corfu-quit)
             ("SPC"   . corfu-insert-separator))
  )


(bundle cape :type github :pkgname "minad/cape")
(use-package cape
  :init
  (add-to-list 'completion-at-point-functions 'cape-keyword t)
  (add-to-list 'completion-at-point-functions 'cape-symbol t)
  (add-to-list 'completion-at-point-functions 'cape-file t))

