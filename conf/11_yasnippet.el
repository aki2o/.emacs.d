(bundle yasnippet)
(use-package yasnippet
  :commands (yas-load-directory)
  
  :mode (("/\\.emacs\\.d/snippets/" . snippet-mode))
  
  :init

  (setq yas-fallback-behavior 'call-other-command)
  ;; (setq yas-next-field-key "TAB")
  (setq yas-indent-line 'auto) ;展開するときにインデントするか
  (setq yas-wrap-around-region t) ;選択領域を$0の位置に挿入するか
  (setq yas-triggers-in-field t) ;再帰的に展開するか
  (setq yas-prompt-functions '(yas-completing-prompt))
  ;; (setq yas-text-popup-function #'yas-dropdown-list-popup-for-template)
  (setq yas-snippet-dirs (list (locate-user-emacs-file "snippets")))

  (yas-global-mode 1)

  :config

  (define-key yas-minor-mode-map (kbd "SPC") 'yas-expand)
  
  (add-hook 'snippet-mode-hook 'turn-on-eldoc-mode t)

  (add-hook 'snippet-mode-hook '~snippet-modde-setup t)
  (defun ~snippet-modde-setup ()
    (setq moccur-grep-default-mask (mmask-get-regexp-string 'snippet-mode)))
  
  (defun ~yas-expand-link (key)
    "Hyperlink function for yasnippet expansion."
    (delete-region (point-at-bol) (1+ (point-at-eol)))
    (insert key)
    (yas-expand))

  (defun ~yas-expand-link-choice (&rest keys)
    "Hyperlink to select yasnippet template."
    (~yas-expand-link (completing-read "Select template: " keys nil t)))

  (defun ~yas-expand-from-key (key)
    "expand snippet from key."
    (insert key)
    (yas-expand))

  (defvar ~yas-register-hash nil)
  (make-variable-buffer-local '~yas-register-hash)

  (defun ~yas-set-register (value &optional key)
    "set register for yasnippet."
    (when (not (hash-table-p ~yas-register-hash))
      (setq ~yas-register-hash (make-hash-table :test 'equal)))
    (puthash (concat "key" key) value ~yas-register-hash) "")

  (defun ~yas-get-register (&optional key)
    "get register for yasnippet."
    (cond ((hash-table-p ~yas-register-hash)
           (gethash (concat "key" key) ~yas-register-hash))
          (t
           "")))

  (defun ~yas-expand-oneshot-snippet-with-region (start end)
    (interactive "r")
    (loop with buf = (buffer-substring-no-properties start end)
          with lf = (read-string "Line End (\\n): " nil '() "\n")
          with delim = (read-string "Variable Separator Regexp (\\s-+): " nil '() "\\s-+")
          with marker = (set-marker (make-marker) end)
          initially (delete-region start end)
          for line in (reverse (split-string buf lf))
          do (yas-expand-snippet yas-oneshot-snippet start)
          do (loop for v in (split-string line delim)
                   do (progn (insert v)
                             (yas-next-field)))
          do (goto-char marker)))

  ;; ;; 展開後インデントする
  ;; (defun ~yas-indent-snippet ()
  ;;   (indent-region yas-snippet-beg yas-snippet-end)
  ;;   (indent-according-to-mode))
  ;; (add-hook 'yas-after-exit-snippet-hook 'yas-indent-snippet t)

  (use-package auto-complete
    :config
    (add-hook 'snippet-mode-hook 'ac-emacs-lisp-mode-setup t))

  (defun ~yas-choose-value (sym)
    (yas-choose-value (symbol-value (intern (format "~yas-%s" sym)))))

  (defvar ~yas-rspec-matchers
    '("eq " "eql " "equal " "include " "cover " "match()" "start_with " "end_with "
      "throw_symbol()" "raise_error()" "receive"
      "be " "be_a_kind_of " "be_an_instance_of " "be_truthy" "be_falsey" "be_nil"
      "be > " "be >= " "be < " "be <= " "be_" "have_"))
  
  (defvar ~yas-rspec-mocks
    '("allow" "expect" "allow_any_instance_of" "expect_any_instance_of"))

  (defvar ~yas-rspec-mock-args
    '(":any_args" ":no_args" ":anything" ":string" ":numeric" ":boolean"
      "hash_including()" "hash_not_including()" "duck_type()"))
  )


;; (bundle anything-c-yasnippet :url "http://svn.coderepos.org/share/lang/elisp/anything-c-yasnippet/anything-c-yasnippet.el")
;; (use-package anything-c-yasnippet

;;   :bind* (("C-x a y" . anything-c-yas-complete))
  
;;   :init
  
;;   ;; スペース区切りで絞り込めるようにする デフォルトは nil
;;   (setq anything-c-yas-space-match-any-greedy t)

;;   ;; (add-to-list 'yas-extra-mode-hooks 'ruby-mode-hook)
;;   ;; (add-to-list 'yas-extra-mode-hooks 'cperl-mode-hook)

;;   )

