(use-package ruby-mode
  :defer t
  :custom ((ruby-indent-level 2)
           (ruby-indent-tabs-mode nil)
           (ruby-insert-encoding-magic-comment nil)
           (ruby-deep-indent-paren-style nil))
  :init
  (with-eval-after-load 'mmask
    (mmask-regist-name 'ruby-mode "Gemfile" "Rakefile" "Guardfile" "Capfile" "Vagrantfile" ".Brewfile")
    (mmask-regist-extension-with-icase 'ruby-mode "rake" "ru" "gemspec" "ruby"))

  :config
  (~browse-document-defun-for ruby "https://docs.ruby-lang.org/en/master/"
    :body (concat (nth 0 words) ".html"))
  (~browse-document-defun-for ruby "https://www.rubydoc.info/"
    :name gem
    :path "find/gems"
    :body (concat "?q=" (mapconcat 'identity words "+")))
  (~browse-document-defun-for ruby "https://api.rubyonrails.org/"
    :name rails)

  (~add-setup-hook 'ruby-mode
    ;; (remove-hook 'before-save-hook 'ruby-mode-set-encoding) ; encodingを自動挿入しないようにする
    (define-key ruby-mode-map (kbd "C-c e") '~ruby-mode-set-encoding)

    (setq ~tidy-code-current-function '~ruby-rubocop-apply-to-current)
    (setq ~tidy-code-diff-files-function '~ruby-rubocop-apply-to-diff-files)

    (electric-indent-local-mode 0)

    (add-function :before (local 'syntax-propertize-function) '~ruby-syntax-propertize-function)

    (when (functionp '~lsp-deferred)
      (~lsp-deferred)
      (setq lsp-completion-enable nil)
      (add-to-list 'lsp-enabled-clients 'ruby-ls)))

  (~add-setup-hook-after-load 'flex-autopair 'ruby-mode
    (add-to-list 'flex-autopair-pairs '(?| . ?|))
    (add-to-list 'flex-autopair-pairs '(?| . ?|))
    (setq flex-autopair-user-conditions-high
          '(((string-match " do +\\'" (buffer-substring (point-at-bol) (point))) . pair)))
    (flex-autopair-reload-conditions))

  (~add-setup-hook-after-load 'mmask 'ruby-mode
    (setq moccur-grep-default-mask (mmask-get-regexp-string 'ruby-mode)))

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

  (defalias '~ruby-syntax-propertize-function
    (syntax-propertize-rules
     ;; 文字列2重展開があるとシンタックスハイライトがおかしくなるので、 ruby-expression-expansion-re を修正したやつを追加
     ("\\(?:[^\\]\\|\\=\\)\\(\\\\\\\\\\)*\\(#{[^{^}]*#{[^}]*}[^}]*}\\)\\|\\(#\\({[^}\n\\\\]*\\(\\\\.[^}\n\\\\]*\\)*}\\|\\(\\$\\|@\\|@@\\)\\(\\w\\|_\\)+\\|\\$[^a-zA-Z \n]\\)\\)"
      (0 (ignore (ruby-syntax-propertize-expansion))))))
  )

(defun ~ruby-mode-set-encoding ()
  (interactive)
  (ruby-mode-set-encoding))

(defun ~ruby-rubocop-apply (&rest path)
  (~dockerize-shell-command (format "bundle exec rubocop -a %s" (mapconcat 'shell-quote-argument path " "))))

(defun ~ruby-rubocop-apply-to-current ()
  (interactive)
  (~ruby-rubocop-apply (~projectile-relative-path (current-buffer))))

(defun ~ruby-rubocop-apply-to-diff-files ()
  (interactive)
  (apply '~ruby-rubocop-apply (~git-diff-path-list (current-buffer))))

(defun ~ruby-rails-show-routes (keyword)
  (interactive
   (list (read-string "Query: ")))
  (shell-command (format "bundle exec rake routes | grep %s"
                         (shell-quote-argument keyword))))


(use-package ruby-block
  :defer t
  :custom ((ruby-block-highlight-toggle t))
  :hook (ruby-mode . ruby-block-mode))


(use-package ruby-end
  :defer t
  :hook (ruby-mode . ruby-end-mode)
  :config
  (unbind-key (read-kbd-macro ruby-end-expand-ret-key) ruby-end-mode-map))


(use-package inf-ruby
  :defer t
  :custom ((inf-ruby-default-implementation "pry")
           (inf-ruby-eval-binding "Pry.toplevel_binding")
           (inf-ruby-console-environment "development"))
  :hook (ruby-mode . inf-ruby-minor-mode)
  :config
  (add-hook 'inf-ruby-mode-hook 'ansi-color-for-comint-mode-on t))


(use-package projectile-rails
  :defer t
  :hook (projectile-mode . projectile-rails-on)
  :init
  ;; (setq rake-completion-system 'helm)

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

  ;; p-r

  (defun rake--root ()
    (directory-file-name (file-truename (locate-dominating-file default-directory "Rakefile"))))

  ;; 複数の階層が分かれている場合、目的のディレクトリのファイルが出なかったので、ファイルパス全体を選択肢に出すようにしてる
  (defun projectile-rails-find-current-view ()
    "Find a template for the current resource."
    (interactive)
    (projectile-rails-find-current-resource "app/views/"
                                            "\\(.*${plural}/[^/]+\\)$"
                                            'projectile-rails-find-view)))


;; void-variable docker-container-attach とエラーになるようになってしまってたので、一旦コメントアウト
;; (use-package docker-projectile-rails
;;   :straight (:host github :repo "aki2o/emacs-docker-projectile-rails" :files ("*.el") :no-byte-compile nil)
;;   :after (projectile-rails)
;;   :init
;;   (use-package docker :defer t)
;;   :config
;;   (docker-projectile-rails:activate))


(use-package rspec-mode
  :after (ruby-mode)
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
               (request-spec-relative-path (replace-regexp-in-string "_controller_spec\\.rb\\'" "_spec.rb" spec-relative-path))
               (request-spec-path (concat spec-root "/requests" request-spec-relative-path)))
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
                         "_controller.rb"
                         (substring spec-path (length request-spec-root))))))))))


(use-package yard-mode
  :defer t
  :hook (ruby-mode . yard-mode))


(use-package rake
  :defer t)
