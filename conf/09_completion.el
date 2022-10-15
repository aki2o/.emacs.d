(define-key minibuffer-local-map (kbd "C-M-p") 'previous-matching-history-element)
(define-key minibuffer-local-map (kbd "C-M-n") 'next-matching-history-element)


(bundle vertico)
(use-package vertico
  :custom ((vertico-count 25))
  :init
  (vertico-mode)
  (add-hook 'minibuffer-setup-hook 'vertico-repeat-save)
  :config
  (define-key vertico-map (kbd "C-S-j") 'vertico-scroll-up)
  (define-key vertico-map (kbd "C-S-k") 'vertico-scroll-down)
  (define-key vertico-map (kbd "C-S-h") 'vertico-directory-delete-word)
  (define-key vertico-map (kbd "C-S-l") 'vertico-insert))


(bundle orderless)
(use-package orderless
  :custom ((orderless-component-separator 'orderless-escapable-split-on-space)
           (orderless-matching-styles '(orderless-regexp))
           (orderless-style-dispatchers '(~orderless-dispatcher-bang ~orderless-dispatcher-quote ~orderless-dispatcher-caret)))
  :init
  (setq completion-styles '(orderless basic))
  ;; (setq completion-category-defaults nil)
  (setq completion-category-overrides '((buffer (styles orderless basic))))
  (setq read-file-name-completion-ignore-case t)
  (setq read-buffer-completion-ignore-case t)
  (setq completion-ignore-case t)

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


(bundle marginalia)
(use-package marginalia
  :bind (:map minibuffer-local-map
              ("C-;" . marginalia-cycle))
  :init
  (marginalia-mode))

