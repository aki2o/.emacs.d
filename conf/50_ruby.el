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
  
  (defun ~ruby-mode-set-encoding ()
    (interactive)
    (ruby-mode-set-encoding))

  (defun ~ruby-rubocop-apply (&rest path)
    (let ((cmd (format "bundle exec rubocop -a %s" (mapconcat 'shell-quote-argument path " "))))
      (if (~docker-context-p (current-buffer))
          (docker-run:exec 'shell-command (format "/bin/bash -l -c '%s'" cmd))
        (shell-command cmd))))

  (defun ~ruby-rubocop-apply-to-current ()
    (interactive)
    (let* ((root-path (projectile-project-root))
           (re (rx-to-string `(and bos ,root-path)))
           (filepath (expand-file-name (buffer-file-name)))
           (path (replace-regexp-in-string re "" filepath)))
      (~ruby-rubocop-apply path)))

  (defun ~ruby-rubocop-apply-to-diff-files ()
    (interactive)
    (apply '~ruby-rubocop-apply (~git-diff-path-list (current-buffer))))

  (defun ~ruby-setup-mode ()
    ;; (remove-hook 'before-save-hook 'ruby-mode-set-encoding) ; encodingを自動挿入しないようにする
    (define-key ruby-mode-map (kbd "C-c e") '~ruby-mode-set-encoding)
    (define-key ruby-mode-map (kbd "M-/") '~ruby-rubocop-apply-to-current)
    (define-key ruby-mode-map (kbd "C-M-/") '~ruby-rubocop-apply-to-diff-files)
    (electric-indent-local-mode 0))
  (add-hook 'ruby-mode-hook '~ruby-setup-mode t)

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
    (unbind-key (read-kbd-macro ruby-end-expand-ret-key) ruby-end-mode-map)
    (add-hook 'ruby-mode-hook 'ruby-end-mode t))

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
  (setq inf-ruby-console-environment "development")
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

  (defun robe-eldoc ()) ;; 重いので、やらない
  
  (use-package docker-robe
    :config
    (docker-robe:activate))
  
  (use-package pophint-config
    :config
    (pophint-config:set-tag-jump-command robe-jump :point-arg-index 0))

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
(bundle aki2o/emacs-docker-projectile-rails :name docker-projectile-rails :depends (docker))
(use-package projectile-rails
  :defer t
  :init

  (setq rake-completion-system 'helm)

  (add-hook 'projectile-mode-hook 'projectile-rails-on)
  (when projectile-mode
    (projectile-mode)
    (projectile-mode))

  :config
  
  (~projectile-switchable-project-commandize projectile-rails-find-controller)
  (~projectile-switchable-project-commandize projectile-rails-find-model)
  (~projectile-switchable-project-commandize projectile-rails-find-view)
  (~projectile-switchable-project-commandize projectile-rails-find-mailer)
  (~projectile-switchable-project-commandize projectile-rails-find-spec)
  (~projectile-switchable-project-commandize projectile-rails-find-test)
  (~projectile-switchable-project-commandize projectile-rails-find-fixture)
  (~projectile-switchable-project-commandize projectile-rails-find-stylesheet)
  (~projectile-switchable-project-commandize projectile-rails-find-javascript)
  (~projectile-switchable-project-commandize projectile-rails-find-locale)
  (~projectile-switchable-project-commandize projectile-rails-find-initializer)
  (~projectile-switchable-project-commandize projectile-rails-find-job)
  (~projectile-switchable-project-commandize projectile-rails-find-migration)
  (~projectile-switchable-project-commandize projectile-rails-find-environment)
  (~projectile-switchable-project-commandize projectile-rails-find-serializer)
  (~projectile-switchable-project-commandize projectile-rails-find-lib)
  (~projectile-switchable-project-commandize projectile-rails-find-feature)
  (~projectile-switchable-project-commandize projectile-rails-find-log)
  (~projectile-switchable-project-commandize projectile-rails-find-layout)
  (~projectile-switchable-project-commandize projectile-rails-find-rake-task)
  (~projectile-switchable-project-commandize projectile-rails-find-validator)

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

  (use-package docker-projectile-rails
    :config
    (docker-projectile-rails:activate))

  ;; p-r

  (defun rake--root ()
    (directory-file-name (file-truename (locate-dominating-file default-directory "Rakefile"))))
  
  ;; hashの値は (concat dir file) ってなってたけど、 app/views/app/views/hoge.html とかなって動いてない
  (defun projectile-rails-choices (dirs)
    "Uses `projectile-rails-dir-files' function to find files in directories.

The DIRS is list of lists consisting of a directory path and regexp to filter files from that directory.
Optional third element can be present in the DIRS list. The third element will be a prefix to be placed before
the filename in the resulting choice.
Returns a hash table with keys being short names (choices) and values being relative paths to the files."
    (let ((hash (make-hash-table :test 'equal)))
      (loop for (dir re prefix) in dirs do
            (loop for file in (projectile-rails-dir-files (projectile-rails-expand-root dir)) do
                  (when (string-match re file)
                    (puthash
                     (concat (or prefix "") (match-string 1 file))
                     file
                     hash))))
      hash))

  ;; 複数の階層が分かれている場合、目的のディレクトリのファイルが出なかったので、ファイルパス全体を選択肢に出すようにしてる
  (defun projectile-rails-find-current-view ()
    "Find a template for the current resource."
    (interactive)
    (projectile-rails-find-current-resource "app/views/"
                                            "\\(.*${plural}/[^/]+\\)$"
                                            'projectile-rails-find-view))

  )


(bundle rspec-mode)
(use-package rspec-mode
  :defer t
  :init
  (custom-set-variables '(rspec-use-rake-flag nil))

  :config
  ;; controller -> request_spec or controller_spec をよしなにする
  (defadvice rspec-spec-file-for (after ~rspec-selectize activate)
    (let* ((spec-root (rspec-spec-directory (ad-get-arg 0)))
           (controller-spec-root (concat spec-root "/controllers")))
      (when (and
             (< (length controller-spec-root) (length ad-return-value))
             (equal
              controller-spec-root
              (substring ad-return-value 0 (length controller-spec-root))))
        (let* ((spec-relative-path (substring ad-return-value (length controller-spec-root)))
               (request-spec-path (concat spec-root "/requests" spec-relative-path)))
          (cond ((or (and (file-exists-p ad-return-value)
                          (file-exists-p request-spec-path))
                     (and (not (file-exists-p ad-return-value))
                          (not (file-exists-p request-spec-path))))
                 (setq ad-return-value
                       (completing-read "Select: " (list ad-return-value request-spec-path) nil t)))
                ((file-exists-p request-spec-path)
                 (setq ad-return-value request-spec-path)))))))

  ;; request_spec -> controller ができない対応
  (defadvice rspec-target-file-for (after ~rspec-adjust-contoller activate)
    (when (not ad-return-value)
      (let* ((spec-path (ad-get-arg 0))
             (spec-root (rspec-spec-directory spec-path))
             (request-spec-root (concat spec-root "/requests")))
        (when (and
               (< (length request-spec-root) (length spec-path))
               (equal
                request-spec-root
                (substring spec-path 0 (length request-spec-root))))
          (setq ad-return-value
                (concat (rspec-project-root spec-path)
                        "/app/controllers"
                        (replace-regexp-in-string
                         "_spec\\.rb\\'"
                         ".rb"
                         (substring spec-path (length request-spec-root)))))))))
  )


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

