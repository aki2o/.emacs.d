(bundle lsp-mode)
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
           (lsp-completion-provider :none))
  :defer t
  :config
  (with-eval-after-load 'cape
    (add-hook 'lsp-completion-mode-hook '~lsp-cape-setup))

  (with-eval-after-load 'which-key
    (add-hook 'lsp-mode 'lsp-enable-which-key-integration))
  )

(defun ~lsp-cape-setup ()
  (add-to-list 'completion-at-point-functions 'lsp-completion-at-point))


(bundle lsp-ui)
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


(bundle lsp-docker)
(use-package lsp-docker
  :after (lsp-mode))


;; (use-package lsp-treemacs
;;   :defer t
;;   :commands lsp-treemacs-errors-list
;;   :after (lsp-mode))


;; (bundle unicode-escape :type github :pkgname "kosh04/unicode-escape.el")
(bundle unicode-escape)
(bundle company)
(bundle company-tabnine)
(use-package company-tabnine
  :after (lsp-mode)
  :config
  (with-eval-after-load 'cape
    (add-hook 'lsp-completion-mode-hook '~company-tabnine-cape-setup)))

(defun ~company-tabnine-cape-setup ()
  (setq completion-at-point-functions (-remove-item 'lsp-completion-at-point completion-at-point-functions))

  (add-to-list 'completion-at-point-functions
               (cape-capf-buster
                (cape-super-capf
                 'lsp-completion-at-point
                 (cape-company-to-capf 'company-tabnine)))))

