;; -*- coding: utf-8 -*-

(bundle ruby-block)
(bundle ruby-end)
(bundle rake)

(use-package ruby-mode
  
  :defer t
  
  :init
  
  (mmask-regist-name 'ruby-mode "Gemfile" "Rakefile" "Guardfile" "Capfile" "Vagrantfile")
  (mmask-regist-extension-with-icase 'ruby-mode "rake" "ru" "gemspec" "ruby")

  (setq ruby-indent-level 2)
  (setq ruby-indent-tabs-mode nil)
  (setq ruby-insert-encoding-magic-comment nil)
  (setq ruby-deep-indent-paren-style nil)

  :config
  
  (defun ~ruby-setup-mode ()
    ;; (remove-hook 'before-save-hook 'ruby-mode-set-encoding) ; encodingを自動挿入しないようにする
    (define-key ruby-mode-map (kbd "C-c e") '~ruby-mode-set-encoding)
    (electric-indent-local-mode 0))
  (add-hook 'ruby-mode-hook '~ruby-setup-mode t)

  (defun ~ruby-mode-set-encoding ()
    (interactive)
    (ruby-mode-set-encoding))

  ;; 閉じ括弧のインデントをイイ感じにする
  (defadvice ruby-indent-line (after ~unindent-closing-paren activate)
    (let ((column (current-column))
          indent offset)
      (save-excursion
        (back-to-indentation)
        (let ((state (syntax-ppss)))
          (setq offset (- column (current-column)))
          (when (and (eq (char-after) ?\))
                     (not (zerop (car state))))
            (goto-char (cadr state))
            (setq indent (current-indentation)))))
      (when indent
        (indent-line-to indent)
        (when (> offset 0) (forward-char offset)))))
  
  (use-package ruby-block
    :config
    (setq ruby-block-highlight-toggle t)
    (add-hook 'ruby-mode-hook 'ruby-block-mode t))

  (use-package ruby-end
    :config
    (unbind-key (read-kbd-macro ruby-end-expand-ret-key) ruby-end-mode-map))

  (use-package flycheck
    :init
    (defun ~ruby-setup-flycheck ()
      (add-to-list 'flycheck-disabled-checkers 'ruby-rubocop) ; rubocopのチェックは自動ではさせない
      (flycheck-mode t))
    (add-hook 'ruby-mode-hook '~ruby-setup-flycheck t))

  (use-package flex-autopair
    :init
    (defun ~ruby-setup-flex-autopair ()
      (add-to-list 'flex-autopair-pairs '(?| . ?|))
      (add-to-list 'flex-autopair-pairs '(?| . ?|))
      (setq flex-autopair-user-conditions-high
            '(((string-match " do +\\'" (buffer-substring (point-at-bol) (point))) . pair)))
      (flex-autopair-reload-conditions))
    (add-hook 'ruby-mode-hook '~ruby-setup-flex-autopair t))

  (use-package color-moccur

    :init
    (defun ~ruby-setup-color-moccur ()
      (setq moccur-grep-default-mask (mmask-get-regexp-string 'ruby-mode)))
    (add-hook 'ruby-mode-hook '~ruby-setup-color-moccur t))

  )


(bundle inf-ruby)
(use-package inf-ruby
  :defer t
  :init
  (setq inf-ruby-default-implementation "pry")
  (setq inf-ruby-eval-binding "Pry.toplevel_binding")
  (add-hook 'ruby-mode-hook 'inf-ruby-minor-mode t)
  :config
  (add-hook 'inf-ruby-mode-hook 'ansi-color-for-comint-mode-on t))


(bundle aki2o/robe :branch "feature-not-merged")
(bundle aki2o/emacs-docker-robe :name docker-robe)
(use-package robe
  :defer t
  
  :init
  
  (setq robe-completing-read-func 'completing-read)

  (defvar ~robe-enabled-modes '(ruby-mode slim-mode))

  (loop for mode in ~robe-enabled-modes
        for hook = (intern-soft (concat (symbol-name mode) "-hook"))
        if (and hook
                (symbolp hook))
        do (add-hook hook 'robe-mode t))

  :config
  
  (define-key robe-mode-map (kbd "M-,")  nil)
  (define-key robe-mode-map (kbd "M-.")  nil)
  (define-key robe-mode-map (kbd "C-'")  'robe-doc)
  (define-key robe-mode-map (kbd "C-\"") 'robe-doc)
  (define-key robe-mode-map (kbd "C->")  'robe-jump)

  (use-package docker-robe
    :config
    (docker-robe:activate))
  
  (use-package pophint-config
    :config
    (pophint-config:set-tag-jump-command robe-jump))

  (defadvice robe-complete-thing (after ~robe-reduce-thing activate)
    (setq ad-return-value
          (-remove (lambda (e) (string-match "=\\'" e)) ad-return-value)))
  
  (defadvice ac-word-candidates (after ~robe-reduce-thing activate)
    (when (memq major-mode ~robe-enabled-modes)
      (setq ad-return-value
            (-remove (lambda (e) (string-match ":\\'" e)) ad-return-value))))
  
  (defvar ac-source-robe-word
    '((available . ac-robe-available)
      (candidates . ac-robe-candidates)
      (document . ac-robe-doc)
      (symbol . "w")
      (requires . 2)
      (cache)))

  (defvar ac-source-robe-member
    '((available . ac-robe-available)
      (candidates . ac-robe-candidates)
      (document . ac-robe-doc)
      (symbol . "m")
      (prefix . "\\.\\([a-zA-Z0-9_]*\\)")
      (requires . 1)
      (cache)))

  (defvar ~robe-ac-variable-index nil)
  (defvar ~robe-ac-symbol-index nil)

  (defun ~robe-enabled-buffer-p (buffer)
    (memq (buffer-local-value 'major-mode buffer) ~robe-enabled-modes))

  (defun ~robe-ac-update-index (buffer index-var-symbol re &optional force)
    (with-current-buffer buffer
      (unless (local-variable-p index-var-symbol)
        (make-local-variable index-var-symbol))
      (when (and (or force
                     (not (car (symbol-value index-var-symbol))))
                 (< (buffer-size) 1048576))
        (set index-var-symbol
             (cons t
                   (save-excursion
                     (-distinct
                      (loop initially (goto-char (point-min))
                            while (re-search-forward re nil t)
                            collect (match-string-no-properties 1)))))))))
    
  (defun ~robe-ac-update-index-all (index-var-symbol re)
    (dolist (buffer (buffer-list))
      (when (or ac-fuzzy-enable
                (and (not (eq buffer (current-buffer)))
                     (~robe-enabled-buffer-p buffer)))
        (~robe-ac-update-index buffer index-var-symbol re))))

  (defun ~robe-ac-indexed-candidates (index-var-symbol re)
    (loop initially (unless ac-fuzzy-enable
                      (~robe-ac-update-index (current-buffer) index-var-symbol re t))
          for buffer in (buffer-list)
          if (and (or (not (integerp ac-limit))
                      (< (length candidates) ac-limit))
                  (~robe-enabled-buffer-p buffer))
          append (funcall ac-match-function
                          ac-prefix
                          (and (local-variable-p index-var-symbol buffer)
                               (cdr (buffer-local-value index-var-symbol buffer))))
          into candidates
          finally return candidates))

  (defun ~robe-ac-prefix-on-code (re)
    (when (and (not (eq (get-text-property (point) 'face) 'font-lock-comment-face))
               (re-search-backward (concat re "\\=") nil t))
      (match-beginning 1)))
  
  (defvar ac-source-robe-variable
    '((init . (~robe-ac-update-index-all
               '~robe-ac-variable-index "[@$]\\([a-zA-Z0-9_]+\\)"))
      (candidates . (~robe-ac-indexed-candidates
                     '~robe-ac-variable-index "[@$]\\([a-zA-Z0-9_]+\\)"))
      (symbol . "v")
      (prefix . (~robe-ac-prefix-on-code "[@$]\\([a-zA-Z0-9_]*\\)"))
      (requires . 0)))
  
  (defvar ac-source-robe-symbol
    '((init . (~robe-ac-update-index-all
               '~robe-ac-symbol-index "[^:][:]\\([a-z0-9_]+\\)"))
      (candidates . (~robe-ac-indexed-candidates
                     '~robe-ac-symbol-index "[^:][:]\\([a-z0-9_]+\\)"))
      (symbol . "s")
      (prefix . (~robe-ac-prefix-on-code "[^:][:]\\([a-z0-9_]*\\)"))
      (requires . 0)))

  (defun ~robe-setup ()
    (ac-robe-setup)
    (set (make-local-variable 'ac-ignore-case) nil)
    (add-to-list 'ac-sources 'ac-source-robe-word)
    (add-to-list 'ac-sources 'ac-source-robe-member)
    (add-to-list 'ac-sources 'ac-source-robe-variable)
    (add-to-list 'ac-sources 'ac-source-robe-symbol)
    (turn-on-eldoc-mode))
  
  (add-hook 'robe-mode-hook '~robe-setup t)

  )


(bundle projectile-rails)
(use-package projectile-rails
  :defer t
  :init
  
  (add-hook 'projectile-mode-hook 'projectile-rails-on)
  (when projectile-mode
    (projectile-mode)
    (projectile-mode))

  :config
  
  (define-key projectile-command-map (kbd "r") 'projectile-rails-command-map)
  (~projectile-switchable-project-commandize projectile-rails-find-controller)
  (~projectile-switchable-project-commandize projectile-rails-find-model)
  (~projectile-switchable-project-commandize projectile-rails-find-view)
  (~projectile-switchable-project-commandize projectile-rails-find-mailer)
  (~projectile-switchable-project-commandize projectile-rails-find-spec)
  (~projectile-switchable-project-commandize projectile-rails-find-test)
  (~projectile-switchable-project-commandize projectile-rails-find-fixture)
  (~projectile-switchable-project-commandize projectile-rails-find-stylesheet)
  (~projectile-switchable-project-commandize projectile-rails-find-javascript)

  (defun ~projectile-rails-ag-current-partial-view ()
    (interactive)
    (let* ((root-path (projectile-rails-root))
           (re (when root-path
                 (rx-to-string `(and bos ,root-path "app/views/"))))
           (view-name (when (buffer-file-name)
                        (replace-regexp-in-string
                         "\\`_?\\([^.]+\\).+\\'"
                         "\\1"
                         (file-name-nondirectory (buffer-file-name)))))
           (view-dir (when (buffer-file-name)
                       (file-name-directory (expand-file-name (buffer-file-name))))))
      (cond ((or (not root-path)
                 (not view-dir))
             (error "Not file in rails project."))
            (current-prefix-arg
             (ag-regexp (format "['\"]%s['\"]" view-name) default-directory))
            (t
             (projectile-ag (replace-regexp-in-string re "" (concat view-dir view-name)))))))

  (define-key projectile-command-map (kbd "s r v") '~projectile-rails-ag-current-partial-view)

  )


(bundle slim-mode)
(use-package slim-mode
  :defer t)


(bundle rspec-mode)
(use-package rspec-mode
  :defer t
  :init
  (custom-set-variables '(rspec-use-rake-flag nil)))


;; ;; gem install rcodetools
;; (defvar ~rcodetools-directory
;;   (replace-regexp-in-string
;;    "\n\\'" ""
;;    (shell-command-to-string
;;     "find `gem environment gemdir` -type d | grep -E 'gems/rcodetools-[0-9.]+$'")))

;; (when ~rcodetools-directory
;;   (add-to-list 'load-path ~rcodetools-directory)
  
;;   (setenv "PATH"
;;           (concat ~rcodetools-directory "/bin:" (getenv "PATH")))

;;   (use-package rcodetools

;;     (setq rct-complete-command-name "bundle exec ruby -S rct-complete --dev --fork --detect-rbtest")
    
;;     :config
    
;;     (defun ~rcodetools-setup ()
;;       (local-set-key (kbd "C-c C-e") 'xmp)
;;       (local-set-key (kbd "<C-return>") 'rct-complete-symbol))
    
;;     (add-hook 'ruby-mode-hook '~rcodetools-setup t))
  
;;   )


;; ;; rsense
;; (use-package rsense
;;   :config
  
;;   (setq rsense-popup-help-key "C-'")
;;   (setq rsense-display-help-buffer-key "C-\"")
;;   (setq rsense-jump-to-definition-key "C->")
;;   (rsense-config-default))


(defun ~ruby-rails-show-routes (keyword)
  (interactive
   (list (read-string "Query: ")))
  (shell-command (format "bundle exec rake routes | grep %s"
                         (shell-quote-argument keyword))))

