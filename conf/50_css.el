(use-package css-mode
  :defer t
  :custom ((cssm-indent-function 'cssm-c-style-indenter))

  :config
  ;;(mmm-add-classes
  ;; '((embedded-css
  ;;    :submode css-mode
  ;;    :front "<style[^>]*>"
  ;;    :back "</style>")))
  ;;
  ;;(mmm-add-mode-ext-class 'nxml-mode nil 'embedded-css)

  (~add-setup-hook 'css-mode
    (setq-local css-indent-offset 2))

  (~add-setup-hook-after-load 'mmask 'css-mode
    (setq moccur-grep-default-mask (mmask-get-regexp-string 'css-mode))))


(use-package scss-mode
  :defer t
  :config
  (~add-setup-hook 'scss-mode
    (setq-local css-indent-offset 2)
    (setq-local scss-compile-at-save nil))

  (~add-setup-hook-after-load 'mmask 'scss-mode
    (setq moccur-grep-default-mask (mmask-get-regexp-string 'scss-mode))))
