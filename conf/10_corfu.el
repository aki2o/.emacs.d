(use-package corfu
  :custom ((corfu-auto t)
           (corfu-auto-deley 0.4)
           (corfu-auto-prefix 4)
           (corfu-preview-current nil)
           (corfu-preselect-first t)
           (corfu-quit-at-boundary 'separator)
           (corfu-quit-no-match nil))
  :hook ((corfu-mode . corfu-popupinfo-mode))

  :init
  (global-corfu-mode)

  :config
  (bind-keys :map corfu-map
             ("C-S-j" . corfu-next)
             ("C-S-k" . corfu-previous)
             ([remap keyboard-escape-quit] . corfu-quit))
  )

(add-to-list 'load-path (concat (file-name-directory (locate-library "corfu")) "extensions"))

(use-package corfu-popupinfo
  :after corfu)


(use-package cape
  :init
  (with-eval-after-load 'minibuffer
    (add-to-list 'completion-at-point-functions '~completion-at-point)))

(defvar ~completion-at-point-functions '(cape-dabbrev cape-keyword))
(make-variable-buffer-local '~completion-at-point-functions)

(defun ~completion-at-point ()
  (apply 'cape-wrap-super ~completion-at-point-functions))


(use-package corfu-prescient
  :after corfu
  :custom ((corfu-prescient-enable-filtering nil)
           (corfu-prescient-override-sorting t))
  :config
  (setq corfu-prescient-completion-styles completion-styles) ;; ensure to be done after configuring completion-styles
  (corfu-prescient-mode 1))


(use-package kind-icon
  :after (corfu)
  :custom ((kind-icon-default-face 'corfu-default))
  :config
  (add-to-list 'corfu-margin-formatters 'kind-icon-margin-formatter))
