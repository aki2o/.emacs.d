(use-package css-mode
  :defer t
  :init
  (setq cssm-indent-function 'cssm-c-style-indenter)

  ;;(mmm-add-classes
  ;; '((embedded-css
  ;;    :submode css-mode
  ;;    :front "<style[^>]*>"
  ;;    :back "</style>")))
  ;;
  ;;(mmm-add-mode-ext-class 'nxml-mode nil 'embedded-css)

  :config
  (add-hook 'css-mode-hook '~css-mode-setup t)
  (defun ~css-mode-setup ()
    (set (make-local-variable 'css-indent-offset) 2)
    (setq ~tidy-code-current-function '~css-tidy)
    ;; color-moccur
    (setq moccur-grep-default-mask (mmask-get-regexp-string 'css-mode))))

(defun ~css-tidy ()
  (interactive)
  (~dockerize-shell-command (format "$(npm bin)/stylelint --fix %s" (shell-quote-argument (~projectile-relative-path (current-buffer))))))


(use-package scss-mode
  :defer t
  :config
  (add-hook 'scss-mode-hook '~scss-mode-setup t)
  (defun ~scss-mode-setup ()
    (set (make-local-variable 'css-indent-offset) 2)
    (set (make-local-variable 'scss-compile-at-save) nil)
    (setq ~tidy-code-current-function '~css-tidy)
    ;; color-moccur
    (setq moccur-grep-default-mask (mmask-get-regexp-string 'scss-mode))))
