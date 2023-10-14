(bundle! projectile)
(use-package projectile)

(setq projectile-keymap-prefix nil)
(setq projectile-enable-caching t)
;; (setq projectile-completion-system 'helm)
(setq projectile-cache-file (concat user-emacs-directory ".projectile.cache"))
(setq projectile-known-projects-file (concat user-emacs-directory ".projectile-bookmarks.eld"))
(setq projectile-require-project-root nil)

(cl-loop for e in '(".plsense" ".tern-project")
         do (add-to-list 'projectile-project-root-files-bottom-up e t))
(cl-loop for e in '("Makefile.PL" "Build.PL")
         do (add-to-list 'projectile-project-root-files e t))
(cl-loop for e in '("group_vars")
         do (add-to-list 'projectile-project-root-files-top-down-recurring e t))
(cl-loop for e in '("blib")
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
;;   (let ((founds (sort (cl-loop for e in (append list
;;                                              projectile-project-root-files
;;                                              projectile-project-root-files-bottom-up)
;;                             for found = (projectile-locate-dominating-file dir e)
;;                             if found collect found)
;;                       (lambda (a b)
;;                         (> (string-width a) (string-width b))))))
;;     (if (and (> (length founds) 1)
;;              ~projectile-maybe-root-selection)
;;         (cl-loop for e in founds
;;               if (y-or-n-p (format "Is project root '%s'?" e))
;;               return e)
;;       (nth 0 founds))))

;; (defadvice projectile-invalidate-cache (around ~select-project-root activate)
;;   (let ((~projectile-maybe-root-selection (when (called-interactively-p) t)))
;;     ad-do-it))

(defun ~projectile-relative-path (buffer)
  (let* ((root-path (projectile-project-root))
         (re (rx-to-string `(and bos ,root-path)))
         (filepath (expand-file-name (buffer-file-name buffer))))
    (replace-regexp-in-string re "" filepath)))

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
            (dir (expand-file-name (projectile-complete-dir (projectile-project-root)) (projectile-project-root))))
        (funcall ag-command search-term dir))
    (error "Ag is not available")))

(defmacro ~projectile-switchable-project-commandize (command)
  `(progn
     (defun ,(intern (format "%s--in-other-project" command)) ()
       (interactive)
       (let ((projectile-switch-project-action (lambda ()
                                                 (call-interactively (setq this-command ',command)))))
         (projectile-switch-project)))))

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


(with-eval-after-load 'pophint-autoloads
  (pophint-thing:advice-thing-at-point-function projectile-symbol-at-point)
  (pophint-thing:defcommand-noadvice projectile-ag))


(with-eval-after-load 'vertico
  (advice-add 'projectile-completing-read :around #'~projectile-let-resumable)
  (advice-add 'projectile-complete-dir :around #'~projectile-let-resumable))

(defun ~projectile-let-resumable (orig &rest args)
  (let* ((vertico-repeat-transformers (if (memq '~projectile-resume-transformer vertico-repeat-transformers)
                                          vertico-repeat-transformers
                                        (append vertico-repeat-transformers '(~projectile-resume-transformer))))
         (~projectile-resumed-command (replace-regexp-in-string "^projectile-" "" (symbol-name this-command)))
         (~projectile-resumed-directory (projectile-project-root)))
    (apply orig args)))

(defvar ~projectile-resumed-command nil)
(defvar ~projectile-resumed-directory nil)

(defun ~projectile-resume-transformer (session)
  (when session
    (list '~projectile-resume
          (mapconcat 'identity (list ~projectile-resumed-command ~projectile-resumed-directory (cadr session)) " ")
          (caddr session))))

(defun ~projectile-resume ()
  (interactive)
  (let* ((session ~vertico-current-session)
         (v (funcall orderless-component-separator (cadr session)))
         (c (pop v))
         (f (intern-soft (format "projectile-%s" c)))
         (dir (pop v))
         (input (mapconcat 'identity v " "))
         (default-directory dir))
    (when (not (commandp f))
      (error "Invalid session : %s" session))
    (setf (nth 1 session) input)
    (unwind-protect
        (call-interactively (setq this-command f))
      (setf (nth 1 session) (mapconcat 'identity (list c dir input) " ")))))


(with-eval-after-load 'consult
  (defun ~projectile-consult-ripgrep ()
    (interactive)
    (funcall '~consult-ripgrep (projectile-project-root)))

  (defun ~projectile-consult-ripgrep-with-directory-select ()
    (interactive)
    (funcall '~consult-ripgrep (expand-file-name (projectile-complete-dir (projectile-project-root)) (projectile-project-root))))

  (~projectile-switchable-project-commandize ~projectile-consult-ripgrep)
  (~projectile-switchable-project-commandize ~projectile-consult-ripgrep-with-directory-select)
  )


;; For p-r

;; ディレクトリなかったら、エラーじゃなくて空リスト返して欲しい
(defadvice projectile-dir-files (around ~check-exists activate)
  (if (file-directory-p (ad-get-arg 0))
      ad-do-it
    (setq ad-return-value nil)))

;; ファイルが無いディレクトリがリストされるようにする
(defun projectile-project-dirs (project)
  (cl-flet ((parent (x)
                    (file-name-directory (directory-file-name x))))
    (delete-dups
     (cl-loop with dirs = (delq nil
                                (mapcar #'file-name-directory (projectile-project-files project)))
              while dirs append dirs
              do (setq dirs (delq nil (mapcar #'parent dirs)))))))

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


(bundle editorconfig)
(use-package editorconfig
  :defer t
  :hook ((prog-mode . editorconfig-mode)))
