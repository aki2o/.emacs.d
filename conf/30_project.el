(bundle! projectile)

(setq projectile-keymap-prefix nil)
(setq projectile-enable-caching t)
(setq projectile-completion-system 'helm)
(setq projectile-cache-file (concat user-emacs-directory ".projectile.cache"))
(setq projectile-known-projects-file (concat user-emacs-directory ".projectile-bookmarks.eld"))

(loop for e in '(".plsense" ".tern-project")
      do (add-to-list 'projectile-project-root-files-bottom-up e t))
(loop for e in '("Makefile.PL" "Build.PL")
      do (add-to-list 'projectile-project-root-files e t))
(loop for e in '("group_vars")
      do (add-to-list 'projectile-project-root-files-top-down-recurring e t))
(loop for e in '("blib")
      do (add-to-list 'projectile-globally-ignored-directories e t))

;; よくわからないけど、動いてないから、常に有効にするので、再定義してみてる
(define-globalized-minor-mode projectile-global-mode
  projectile-mode
  (lambda () (projectile-mode 1)))

(projectile-global-mode)

;; ;; プロジェクトルート探索方法
;; (setq projectile-project-root-files-functions '(~projectile-root-bottom-up))

;; (defvar ~projectile-maybe-root-selection nil)
;; (defun ~projectile-root-bottom-up (dir &optional list)
;;   (let ((founds (sort (loop for e in (append list
;;                                              projectile-project-root-files
;;                                              projectile-project-root-files-bottom-up)
;;                             for found = (projectile-locate-dominating-file dir e)
;;                             if found collect found)
;;                       (lambda (a b)
;;                         (> (string-width a) (string-width b))))))
;;     (if (and (> (length founds) 1)
;;              ~projectile-maybe-root-selection)
;;         (loop for e in founds
;;               if (y-or-n-p (format "Is project root '%s'?" e))
;;               return e)
;;       (nth 0 founds))))

;; (defadvice projectile-invalidate-cache (around ~select-project-root activate)
;;   (let ((~projectile-maybe-root-selection (when (interactive-p) t)))
;;     ad-do-it))

;; コマンド追加
(defun ~projectile-find-root-dir (&optional arg)
  (interactive "P")
  (projectile-maybe-invalidate-cache arg)
  (dired (projectile-project-root)))

(defun ~projectile-ag-with-directory-select (&optional arg)
  (interactive
   (list (read-from-minibuffer
          (projectile-prepend-project-name (format "Ag %ssearch for: " (if current-prefix-arg "regexp " "")))
          (projectile-symbol-at-point))
         current-prefix-arg))
  (if (fboundp 'ag-regexp)
      (let ((ag-command (if arg 'ag-regexp 'ag))
            ;; reset the prefix arg, otherwise it will affect the ag-command
            (current-prefix-arg nil)
            (dir (expand-file-name (projectile-complete-dir) (projectile-project-root))))
        (funcall ag-command search-term dir))
    (error "Ag is not available")))

(defvar ~projectile-switch-project-processing nil)
(defmacro ~projectile-switchable-project-commandize (command)
  `(progn
     (defun ,(intern (format "%s--in-other-project" command)) ()
       (interactive)
       (let ((projectile-switch-project-action (lambda ()
                                                 (call-interactively ',command)))
             (~projectile-switch-project-processing t))
         (projectile-switch-project)))
     (defadvice ,command (around ~projectile-switch-project activate)
       (if (and (not ~projectile-switch-project-processing)
                (interactive-p)
                current-prefix-arg)
           (let ((current-prefix-arg nil))
             (call-interactively ',(intern (format "%s--in-other-project" command))))
         ad-do-it))))

(~projectile-switchable-project-commandize projectile-find-file)
(~projectile-switchable-project-commandize projectile-find-dir)
(~projectile-switchable-project-commandize ~projectile-find-root-dir)
(~projectile-switchable-project-commandize projectile-switch-to-buffer)
(~projectile-switchable-project-commandize projectile-kill-buffers)
(~projectile-switchable-project-commandize projectile-invalidate-cache)
(~projectile-switchable-project-commandize projectile-ag)
(~projectile-switchable-project-commandize ~projectile-ag-with-directory-select)
(~projectile-switchable-project-commandize projectile-multi-occur)
(~projectile-switchable-project-commandize projectile-find-test-file)


(defadvice uniquify-rename-buffer (before ~try-put-projectile-project (item newname))
  (let* ((buff (uniquify-item-buffer item))
         (default-directory (file-name-directory (buffer-file-name buffer)))
         (project-root (projectile-project-root)))
    (when project-root
      (ad-set-arg 1 (format "%s - %s" newname project-root)))))


(use-package pophint
  :config
  (pophint-thing:advice-thing-at-point-function projectile-symbol-at-point)
  (pophint-thing:defcommand-noadvice projectile-ag))


(bundle counsel-projectile)
(use-package counsel
  :config
  (defun ~projectile-counsel-ag ()
    (interactive)
    (counsel-ag (~counsel-initial-input) (projectile-project-root)))

  (defun ~projectile-counsel-ag-with-directory-select ()
    (interactive)
    (let ((dir (expand-file-name (projectile-complete-dir) (projectile-project-root))))
      (counsel-ag (~counsel-initial-input) dir)))
  
  (~projectile-switchable-project-commandize ~projectile-counsel-ag)
  (~projectile-switchable-project-commandize ~projectile-counsel-ag-with-directory-select)
  )


(bundle helm-projectile)
(use-package helm-projectile
  :config
  ;; (setq projectile-completion-system 'helm)
  ;; (helm-projectile-on)

  ;; (~projectile-switchable-project-commandize helm-projectile-find-file)
  ;; (~projectile-switchable-project-commandize helm-projectile-find-dir)
  ;; (~projectile-switchable-project-commandize helm-projectile-switch-to-buffer)
  (~projectile-switchable-project-commandize helm-projectile-ag)
  (~projectile-switchable-project-commandize helm-projectile-rg)
  )


;; For p-r

;; ディレクトリなかったら、エラーじゃなくて空リスト返して欲しい
(defadvice projectile-dir-files (around ~check-exists activate)
  (if (file-directory-p (ad-get-arg 0))
      ad-do-it
    (setq ad-return-value nil)))

(defun projectile-project-root (&optional ignore-cache)
  "Retrieves the root directory of a project if available.
The current directory is assumed to be the project's root otherwise."
  (file-truename
   (let ((dir (file-truename default-directory)))
     (or (--reduce-from
          (or acc
              (let* ((cache-key (format "%s-%s" it dir))
                     (cache-value (when (not ignore-cache)
                                    (gethash cache-key projectile-project-root-cache)))
                     (value (or cache-value
                                (puthash cache-key
                                         (or (funcall it dir) 'no-project-root)
                                         projectile-project-root-cache))))
                (if (eq value 'no-project-root)
                    nil
                  value)))
          nil
          projectile-project-root-files-functions)
         (if projectile-require-project-root
             (error "You're not in a project")
           default-directory)))))

(defun projectile-invalidate-cache (arg)
  "Remove the current project's files from `projectile-projects-cache'.

With a prefix argument ARG prompts for the name of the project whose cache
to invalidate."
  (interactive "P")
  (let ((project-root
         (if arg
             (completing-read "Remove cache for: "
                              (projectile-hash-keys projectile-projects-cache))
           (projectile-project-root t))))
    (remhash project-root projectile-projects-cache)
    (projectile-serialize-cache)
    (when projectile-verbose
      (message "Invalidated Projectile cache for %s."
               (propertize project-root 'face 'font-lock-keyword-face)))))

