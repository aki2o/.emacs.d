(bundle yasnippet)
(use-package yasnippet
  :defer t
  :commands (yas-visit-snippet-file
             yas-insert-snippet
             yas-new-snippet
             yas-reload-all
             yas-load-directory
             ~yas-register-oneshot-snippet
             ~yas-expand-oneshot-snippet)
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

  (defvar ~yas-oneshot-snippet nil)
  (defun ~yas-register-oneshot-snippet (start end)
    (interactive "r")
    (setq ~yas-oneshot-snippet (buffer-substring-no-properties start end))
    (delete-region start end)
    (message "%s" (substitute-command-keys "Press \\[~yas-expand-oneshot-snippet] to expand.")))

  (defun ~yas-expand-oneshot-snippet ()
    (interactive)
    (yas-expand-snippet ~yas-oneshot-snippet (point) (point) nil))

  ;; ;; 展開後インデントする
  ;; (defun ~yas-indent-snippet ()
  ;;   (indent-region yas-snippet-beg yas-snippet-end)
  ;;   (indent-according-to-mode))
  ;; (add-hook 'yas-after-exit-snippet-hook 'yas-indent-snippet t)

  (when (fboundp 'ac-emacs-lisp-mode-setup)
    (add-hook 'snippet-mode-hook 'ac-emacs-lisp-mode-setup t))

  (defun ~yas-choose-value (sym)
    (yas-choose-value (symbol-value (intern (format "~yas-%s" sym)))))

  (defvar ~yas-dummy-history '())
  (defun ~yas-ruby-method-args ()
    (save-excursion
      (let* ((start (loop while (not (eq (get-text-property (point) 'face) 'font-lock-function-name-face))
                          do (goto-char (next-single-property-change (point) 'face))
                          finally return (progn
                                           (goto-char (next-single-property-change (point) 'face))
                                           (point))))
             (end (progn (forward-sexp)
                         (point)))
             (args (loop for e in (split-string (buffer-substring-no-properties start end) ",")
                         for name = (when (string-match (rx bos (or "(" (* space)) (group (+ (any "a-zA-Z0-9_")))) e)
                                      (match-string 1 e))
                         if name collect name)))
        (completing-read "Choice or input name:" args nil nil nil '~yas-dummy-history))))

  (defvar ~yas-rspec-matchers
    '("eq " "eql " "equal " "include " "cover " "match " "start_with " "end_with " "contain_exactly()"
      "be " "be_a_kind_of " "be_a " "be_truthy" "be_falsey" "be_nil" "be_present" "be_empty" "be_in []" "be_added()" "be_of_kind()"
      "be > " "be >= " "be < " "be <= " "be_"
      "have_attributes()" "have_received()" "have_been_enqueued" "have_enqueued_job().with()" "have_http_status" "redirect_to" "an_object_having_attributes"
      "throw_symbol()" "raise_error()" "receive" "change {}.from().to()"
      "belong_to" "have_one().dependent()" "have_many().dependent()"
      "validate_presence_of()" "validate_absence_of()"
      "validate_uniqueness_of().scoped_to()"
      "validate_numericality_of().is_greater_than_or_equal_to().is_less_than_or_equal_to().only_integer"
      "validate_length_of().is_at_least().is_at_most().is_equal_to()"
      "validate_inclusion_of().in_array()"))
  
  (defvar ~yas-rspec-mocks
    '("allow" "expect" "allow_any_instance_of" "expect_any_instance_of"))

  (defvar ~yas-rspec-mock-args
    '(":any_args" ":no_args" ":anything" ":string" ":numeric" ":boolean"
      "hash_including()" "hash_not_including()" "duck_type()"))

  (defvar ~yas-mysql-foreign-key-references
    '("RESTRICT" "CASCADE" "SET NULL" "NO ACTION")))

