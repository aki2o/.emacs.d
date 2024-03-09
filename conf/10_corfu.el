(use-package corfu
  :custom ((corfu-auto t)
           (corfu-auto-deley 0.4)
           (corfu-auto-prefix 4)
           (corfu-preview-current nil)
           (corfu-preselect 'first)
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

  (with-eval-after-load 'fussy
    (advice-add 'corfu--capf-wrapper :before 'fussy-wipe-cache))
  (~add-setup-hook-after-load 'fussy 'corfu-mode
    (setq-local fussy-default-regex-fn 'fussy-pattern-first-letter)
    (setq-local fussy-prefer-prefix nil)))

(defun my:corfu-enable-in-minibuffer ()
  (when (local-variable-p 'completion-at-point-functions)
    (setq-local corfu-echo-delay nil)
    (setq-local corfu-popupinfo-delay nil)
    (corfu-mode 1)))

(add-hook 'minibuffer-setup-hook #'my:corfu-enable-in-minibuffer)

(use-package corfu-popupinfo
  :after corfu
  :custom ((corfu-popupinfo-delay '(1.0 . 0.5))
           (corfu-popupinfo-max-width 120)
           (corfu-popupinfo-max-height 20)))


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
