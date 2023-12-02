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
           (lsp-enable-snippet nil)
           (lsp-headerline-breadcrumb-enable nil)
           (lsp-idle-delay 0.5)
           (lsp-completion-provider :none)
           (lsp-warn-no-matched-clients nil))
  :defer t
  :config
  (make-variable-buffer-local 'lsp-enabled-clients)
  (make-variable-buffer-local 'lsp-completion-enable)

  (advice-add 'lsp-completion-at-point :around '~lsp-completion-at-point)

  (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-overrides)) '(substring))

  (~add-setup-hook 'lsp-after-initialize
    (setq ~lsp-initialized t))

  (~add-setup-hook-after-load 'which-key 'lsp-mode
    (lsp-enable-which-key-integration))
  )

(defvar ~lsp-initialized nil)
(make-variable-buffer-local '~lsp-initialized)

(defun ~lsp-completion-at-point (orig &rest args)
  (when ~lsp-initialized
    (cape-wrap-buster orig)))


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


(use-package lsp-docker
  :after (lsp-mode))


;; (use-package lsp-treemacs
;;   :defer t
;;   :commands lsp-treemacs-errors-list
;;   :after (lsp-mode))
