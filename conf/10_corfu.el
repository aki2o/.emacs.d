(bundle corfu :type git :url "git@github.com:minad/corfu.git" :checkout "de565e2a358eacc305420cfb590f07612ee6dfec")
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
             ("C-S-k" . corfu-previous))
  )

(add-to-list 'load-path (concat (file-name-directory (locate-library "corfu")) "extensions"))

(use-package corfu-popupinfo
  :after corfu)


(bundle cape :type github :pkgname "minad/cape" :branch "main")
(use-package cape
  :init
  (with-eval-after-load 'minibuffer
    (add-to-list 'completion-at-point-functions '~completion-at-point-function)))

(defun ~completion-at-point-function ()
  (cape-wrap-super #'cape-dabbrev #'cape-keyword))


(bundle corfu-prescient :type github :pkgname "radian-software/prescient.el")
(use-package corfu-prescient
  :after corfu
  :custom ((corfu-prescient-enable-filtering nil)
           (corfu-prescient-override-sorting t))
  :config
  (setq corfu-prescient-completion-styles completion-styles) ;; ensure to be done after configuring completion-styles
  (corfu-prescient-mode 1))


(bundle kind-icon)
(use-package kind-icon
  :after (corfu)
  :custom ((kind-icon-default-face 'corfu-default))
  :config
  (add-to-list 'corfu-margin-formatters 'kind-icon-margin-formatter))

