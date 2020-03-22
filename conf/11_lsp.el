(bundle lsp-mode)
;; (bundle spinner)
(use-package lsp-mode
  :custom ((lsp-keymap-prefix "H-l")
           (lsp-inhibit-message t)
           (lsp-message-project-root-warning t)
           (lsp-print-io nil)
           (lsp-trace nil)
           (lsp-print-performance nil)
           (lsp-auto-guess-root t)
           (lsp-document-sync-method 'incremental)
           (lsp-response-timeout 5))
  ;; :hook (lsp-mode . lsp-enable-which-key-integration)
  )

(bundle lsp-ui)
(use-package lsp-ui
  :after (lsp-mode)
  :custom ((lsp-ui-sideline-enable nil)
           (lsp-ui-peek-enable nil)
           (lsp-ui-doc-enable t)
           (lsp-ui-doc-delay nil)
           (lsp-ui-doc-header t)
           (lsp-ui-doc-include-signature t)
           (lsp-ui-doc-position 'at-point)
           (lsp-ui-doc-alignment 'frame)
           (lsp-ui-doc-border "white")
           (lsp-ui-doc-max-width 150)
           (lsp-ui-doc-max-height 50)
           (lsp-ui-imenu-enable nil))
  :hook   ((lsp-mode . lsp-ui-mode)
           (lsp-ui-mode . ~lsp-ui-mode-setup))
  :config
  (custom-set-faces
   '(lsp-ui-doc-background ((t :background "gray30"))))

  (defun ~lsp-ui-doc-show ()
    (interactive)
    (let ((lsp-ui-doc-delay 0.1))
      (lsp-ui-doc-show)))
  
  (defun ~lsp-ui-mode-setup ()
    (local-set-key (kbd "C->") 'xref-find-definitions-other-window)
    (local-set-key (kbd "C-<") 'xref-pop-marker-stack)
    (local-set-key (kbd "C-M->") 'xref-find-references)
    (local-set-key (kbd "C-'") '~lsp-ui-doc-show))

  (with-eval-after-load 'pophint
    (pophint-tags:advice-command xref-find-definitions-other-window)))

;; (bundle lsp-treemacs)
;; (use-package lsp-treemacs
;;   :commands lsp-treemacs-errors-list
;;   :after (lsp-mode))

(bundle company-lsp)
(use-package company-lsp
  :after (lsp-mode company)
  :init (push 'company-lsp company-backends)
  :custom ((company-lsp-cache-candidates 'auto)
           (company-lsp-async nil)
           (company-lsp-enable-snippet nil)
           (company-lsp-enable-recompletion t)))

(bundle helm-lsp)
(use-package helm-lsp
  :after (lsp-mode helm))
