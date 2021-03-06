(use-package lsp-mode
  :custom ((lsp-keymap-prefix "C-x l")
           (lsp-signature-render-documentation nil)
           (lsp-signature-auto-activate nil)
           (lsp-inhibit-message t)
           (lsp-message-project-root-warning t)
           (lsp-print-io nil)
           (lsp-trace nil)
           (lsp-print-performance nil)
           (lsp-auto-guess-root t)
           (lsp-document-sync-method 'incremental)
           (lsp-response-timeout 5)
           (lsp-enable-snippet nil))
  :defer t
  ;; :hook (lsp-mode . lsp-enable-which-key-integration)
  )


(use-package lsp-ui
  :after (lsp-mode)
  :custom ((lsp-ui-sideline-enable nil)
           (lsp-ui-flycheck-live-reporting nil)
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
           (lsp-ui-doc-frame . ~lsp-ui-doc-frame-setup))
  :config
  (custom-set-faces
   '(lsp-ui-doc-background ((t :background "gray30"))))

  (setq lsp-ui-doc-frame-parameters
        (append lsp-ui-doc-frame-parameters '((cursor-type . hbar)
                                              (cursor-color . "white"))))

  (defun ~lsp-ui-doc-frame-setup (frame window)
    (with-selected-window window
      (with-selected-frame frame
        (local-set-key (kbd "C-:") 'lsp-ui-doc-unfocus-frame)
        (local-set-key (kbd "C-M-:") '~lsp-ui-doc-dump-on-doc-frame))))

  (defun ~lsp-ui-doc-show ()
    (interactive)
    (let ((lsp-ui-doc-delay 0.1))
      (lsp-ui-doc-show)))

  (defun ~lsp-ui-doc-dump-on-my-frame ()
    (interactive)
    (~lsp-ui-doc-dump (selected-frame)))

  (defun ~lsp-ui-doc-dump-on-doc-frame ()
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
        (setq buffer-read-only t)
        (set-buffer-modified-p nil)
        (rename-buffer buffname)
        (pop-to-buffer (current-buffer))))))


;; (use-package lsp-treemacs
;;   :defer t
;;   :commands lsp-treemacs-errors-list
;;   :after (lsp-mode))


(use-package company-lsp
  :after (lsp-mode company)
  :init (add-to-list 'company-backends 'company-lsp)
  :custom ((company-lsp-cache-candidates 'auto)
           (company-lsp-async nil)
           (company-lsp-enable-snippet nil) ; lsp-enable-snippet とセットで設定する必要がある
           (company-lsp-enable-recompletion nil)))


(use-package helm-lsp
  :commands (helm-lsp-workspace-symbol)
  :after (lsp-mode))
