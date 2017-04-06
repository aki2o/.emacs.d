(add-hook 'sh-mode-hook
          '(lambda ()
             (setq sh-basic-offset 4)
             (setq sh-indentation 4)
             (add-to-list 'ac-sources 'ac-source-yasnippet)
             )
          t)


;; TAGはauto-completeしない
;; ;; auto-complete-etags
;; (add-hook 'shell-script-mode-hook '(lambda () (add-to-list 'ac-sources 'ac-source-etags)) t)


;; ;; flymake
;; (defun flymake-sh-init ()
;;   (let* ((temp-file (flymake-init-create-temp-buffer-copy 'flymake-create-temp-inplace))
;;          (local-file (file-relative-name temp-file (file-name-directory buffer-file-name))))
;;     (list "bash" (list "-n" local-file))))
;; (defun flymake-csh-init ()
;;   (let* ((temp-file (flymake-init-create-temp-buffer-copy 'flymake-create-temp-inplace))
;;          (local-file (file-relative-name temp-file (file-name-directory buffer-file-name))))
;;     (list "csh" (list "-n" local-file))))

;; (push (list (mmask-get-regexp-string 'sh-mode) 'flymake-sh-init) flymake-allowed-file-name-masks)
;; (push '("\\.csh$" flymake-csh-init) flymake-allowed-file-name-masks)

;; (defun flymake-sh-load ()
;;   (interactive)
;;   (setq flymake-err-line-patterns '(("\\(.*\\): line \\([0-9]+\\): \\([^\n]+\\)" 1 2 nil 3)))
;;   (local-set-key (kbd "C-b") 'flymake-goto-next-error-and-popup)
;;   (local-set-key (kbd "C-S-b") 'flymake-goto-prev-error-and-popup)
;;   (flymake-mode t))

;; (add-hook 'sh-mode-hook 'flymake-sh-load t)


(use-package color-moccur
  :defer t
  :config
  (add-hook 'sh-mode-hook
            '(lambda () (setq moccur-grep-default-mask (mmask-get-regexp-string 'sh-mode)))
            t))


(use-package flex-autopair
  :defer t
  :config

  (defun flex-autopair-sh-mode-setup ()
    (setq flex-autopair-default-conditions '(((and (eq major-mode 'sh-mode)
                                                   (eq last-command-event ?\[))
                                              . self)
                                             ((and (eq major-mode 'sh-mode)
                                                   (eq last-command-event ?\())
                                              . self)))
    (flex-autopair-reload-conditions))
  
  (add-hook 'sh-mode-hook 'flex-autopair-sh-mode-setup t))


(use-package key-combo
  :defer t
  :config

  (defun key-combo-sh-mode-setup ()
    (key-combo-define-local (kbd "(") '("() {\n`!!'\n}"))
    (key-combo-define-local (kbd "|") '(" | ")))
  
  (add-hook 'sh-mode-hook 'key-combo-sh-mode-setup t))


