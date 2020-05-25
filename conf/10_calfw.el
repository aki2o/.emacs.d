(use-package calfw
  :defer t
  :config
  (add-hook 'cfw:calendar-mode-hook '~cfw:setup-buffer t)
  
  (defun ~cfw:setup-buffer ()
    (buffer-face-set (font-face-attributes "Migu 2M-9")))

  (defadvice cfw:dest-init-buffer (around ~dbg activate)
    (set-frame-font "Migu 2M-9")
    ad-do-it
    (set-frame-font "Osaka－等幅-12")))
