(use-package java-mode
  :defer t
  
  :config
  
  (defun ~java-mode-setup ()
    (add-to-list 'ac-sources 'ac-source-yasnippet)
    ;; (add-to-list 'ac-sources 'ac-source-gtags))
    (setq moccur-grep-default-mask (rx-to-string `(or ,(mmask-get-regexp-sexp 'java-mode)
                                                      ,(mmask-get-regexp-sexp 'scala-mode)))))
  
  (add-hook 'java-mode-hook '~java-mode-setup t))
  

(bundle meghanada)
(use-package meghanada
  :init
  
  (defun ~meghanada-setup ()
    (meghanada-mode t)
    (add-hook 'before-save-hook 'meghanada-code-beautify-before-save))

  (add-hook 'java-mode-hook '~meghanada-setup t))


(bundle scala-mode)
(use-package scala-mode
  :defer t

  :init
  
  (mmask-regist-extension-with-icase 'scala-mode "scala")
  
  :config

  (defun ~scala-mode-setup ()
    (local-set-key [f1] nil)
    (add-to-list 'ac-sources 'ac-source-yasnippet)
    (local-set-key (kbd ".") 'self-insert-with-ac-trigger-command)
    (setq moccur-grep-default-mask (rx-to-string `(or ,(mmask-get-regexp-sexp 'java-mode)
                                                      ,(mmask-get-regexp-sexp 'scala-mode)))))
  
  (add-hook 'scala-mode-hook '~scala-mode-setup t))

