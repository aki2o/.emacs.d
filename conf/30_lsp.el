(use-package lsp-mode
  :custom ((lsp-keymap-prefix "C-x ;")
           (lsp-signature-function 'lsp-signature-posframe)
           (lsp-signature-render-documentation t)
           (lsp-signature-auto-activate nil)
           (lsp-signature-doc-lines 1000)
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
           (lsp-warn-no-matched-clients t))
  :defer t
  :config
  (unbind-key "C-S-SPC" lsp-mode-map)

  (bind-keys :map lsp-signature-mode-map
             ([remap keyboard-escape-quit] . lsp-signature-stop))

  (make-variable-buffer-local 'lsp-enabled-clients)
  (make-variable-buffer-local 'lsp-completion-enable)

  (advice-add 'lsp-completion-at-point :around '~lsp-completion-at-point)

  (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-overrides)) '(substring))

  (~add-setup-hook 'lsp-mode
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults)) '(orderless))
    (add-to-list '~completion-at-point-functions '~lsp-completion)
    (setq ~dwim-at-point-function '~lsp-hydra/body))

  (~add-setup-hook-after-load 'which-key 'lsp-mode
    (lsp-enable-which-key-integration))

  (with-eval-after-load 'web-mode
    (add-to-list 'lsp--formatting-indent-alist '(web-mode . web-mode-markup-indent-offset))))

(defun ~lsp-deferred ()
  (interactive)
  (when (buffer-file-name)
    (~run-deferred (current-buffer) (lsp))))

(defvar ~lsp-completion-running-p nil)

(defun ~lsp-completion ()
  (let ((~lsp-completion-running-p t))
    (cape-wrap-buster 'lsp-completion-at-point)))

;; lsp-completion-at-point が実行されると、候補が無くてもそこで補完が終ってしまうので、 ~completion-at-point-functions に登録してそっちを使う
(defun ~lsp-completion-at-point (orig &rest args)
  (if ~lsp-completion-running-p
      (apply orig args)
    (~completion-at-point)))

(defhydra ~lsp-hydra (:exit t :hint nil)
  "
_a_: find apropos          _m_: rename            _r_: restart
_i_: find implementation   _h_: organize imports  _q_: shutdown
_t_: find type definition  _;_: toggle signature  _?_: describe session
_d_: find declaration
"
  ("a" xref-find-apropos)
  ("i" lsp-find-implementation)
  ("t" lsp-find-type-definition)
  ("d" lsp-find-declaration)
  ("m" lsp-rename)
  ("h" lsp-organize-imports)
  (";" lsp-toggle-signature-auto-activate)
  ("r" lsp-workspace-restart)
  ("q" lsp-workspace-shutdown)
  ("?" lsp-describe-session))


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
           (lsp-ui-doc-max-height 1000)
           (lsp-ui-imenu-enable nil))
  :hook   ((lsp-mode . lsp-ui-mode)
           (lsp-ui-doc-frame . ~lsp-ui-doc-frame-setup))
  :config
  (~add-setup-hook 'lsp-ui-mode
    (setq ~popup-document-frame-function '~lsp-ui-doc-show)
    (setq ~popup-document-buffer-function '~lsp-ui-doc-dump-on-my-frame)
    (setq ~focus-document-frame-function 'lsp-ui-doc-focus-frame))

  (custom-set-faces
   '(lsp-ui-doc-background ((t :background "gray30"))))

  (setq lsp-ui-doc-frame-parameters
        (append lsp-ui-doc-frame-parameters '((cursor-type . hbar)
                                              (cursor-color . "white"))))

  (with-eval-after-load 'lsp-ui-doc
    (advice-add #'keyboard-escape-quit :before #'lsp-ui-doc--hide-frame)))

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
    (when (not hover)
      (error "There is no contents to hover!"))
    (lsp-ui-doc--hide-frame)
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
      (pop-to-buffer (current-buffer)))))


(use-package lsp-docker
  :after (lsp-mode))


;; (use-package lsp-treemacs
;;   :defer t
;;   :commands lsp-treemacs-errors-list
;;   :after (lsp-mode))
