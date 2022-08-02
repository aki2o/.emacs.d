(use-package vertico
  :init
  (vertico-mode)

  :custom
  ;; (vertico-count 20)
  )


(use-package orderless
  :ensure t

  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion))))
  (orderless-matching-styles '(orderless-regexp))
  (orderless-style-dispatchers '(~orderless-dispatcher-bang ~orderless-dispatcher-quote ~orderless-dispatcher-caret))

  :config

  (defun ~orderless-dispatcher-bang (pattern _index _total)
    (cond
     ((equal "!" pattern)
      '(orderless-literal . ""))
     ((string-prefix-p "!" pattern)
      `(orderless-without-literal . ,(substring pattern 1)))))

  (defun ~orderless-dispatcher-quote (pattern _index _total)
    (cond
     ((equal "'" pattern)
      '(orderless-literal . ""))
     ((string-prefix-p "'" pattern)
      `(orderless-literal . ,(substring pattern 1)))))

  (defun ~orderless-dispatcher-caret (pattern _index _total)
    (cond
     ((equal "^" pattern)
      '(orderless-literal . ""))
     ((string-prefix-p "^" pattern)
      `(orderless-initialism . ,(substring pattern 1)))))
  )


(use-package marginalia
  :bind (:map minibuffer-local-map
              ("C-@" . marginalia-cycle))
  :init
  (marginalia-mode))


(use-package embark
  )
