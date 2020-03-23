(bundle lsp-mode)
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
           (lsp-ui-mode . ~lsp-ui-mode-setup)
           (lsp-ui-doc-frame . ~lsp-ui-doc-frame-setup))
  :config
  (custom-set-faces
   '(lsp-ui-doc-background ((t :background "gray30"))))

  (setq lsp-ui-doc-frame-parameters
        (append lsp-ui-doc-frame-parameters '((cursor-type . hbar)
                                              (cursor-color . "white"))))

  (defun ~lsp-ui-mode-setup ()
    (local-set-key (kbd "C->") 'xref-find-definitions-other-window)
    (local-set-key (kbd "C-<") 'xref-pop-marker-stack)
    (local-set-key (kbd "C-M->") 'xref-find-references)
    (local-set-key (kbd "C-'") '~lsp-ui-doc-show)
    (local-set-key (kbd "C-\"") 'lsp-ui-doc-focus-frame)
    (local-set-key (kbd "C-M-\"") '~lsp-ui-doc-dump-on-my-frame))

  (defun ~lsp-ui-doc-frame-setup (frame window)
    (with-selected-window window
      (with-selected-frame frame
        (local-set-key (kbd "C-\"") 'lsp-ui-doc-unfocus-frame)
        (local-set-key (kbd "C-M-\"") '~lsp-ui-doc-dump-on-doc-frome))))

  (defun ~lsp-ui-doc-show ()
    (interactive)
    (let ((lsp-ui-doc-delay 0.1))
      (lsp-ui-doc-show)))

  (defun ~lsp-ui-doc-dump-on-my-frame ()
    (interactive)
    (~lsp-ui-doc-dump (selected-frame)))

  (defun ~lsp-ui-doc-dump-on-doc-frome ()
    (interactive)
    (~lsp-ui-doc-dump (frame-parent (selected-frame))))

  (defun ~lsp-ui-doc-dump (frame)
    (let* ((lsp-ui-doc-use-childframe nil)
           (symbol (with-selected-frame frame
                     (thing-at-point 'symbol t)))
           (hover (with-selected-frame frame
                    (lsp-request "textDocument/hover" (lsp--text-document-position-params))))
           (bounds (with-selected-frame frame
                     (or (bounds-of-thing-at-point 'symbol) (cons (point) (1+ (point))))))
           (buffname (format "*~lsp-ui-doc %s*" symbol)))
      (lsp-ui-doc--render-buffer
       (-some->> (gethash "contents" hover)
         lsp-ui-doc--extract
         (replace-regexp-in-string "\r" ""))
       symbol)
      (when (get-buffer buffname)
        (kill-buffer buffname))
      (with-current-buffer (lsp-ui-doc--make-buffer-name)
        (setq cursor-type 'hbar)
        (rename-buffer buffname)
        (pop-to-buffer (current-buffer)))))

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
  :after (lsp-mode helm)
  :hook (lsp-mode . ~helm-lsp-setup)
  :config
  (defun ~helm-lsp-setup ()
    (local-set-key (kbd "C-M-.") 'helm-lsp-workspace-symbol)))
