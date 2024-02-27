;; SVN
(use-package dsvn
  :defer t
  :commands (svn-status svn-update))


;; Git
(setenv "GIT_PAGER" "")


(use-package git-modes
  :defer t)


(use-package magit
  :after (persp-mode)
  :config
  (unbind-key "j" magit-status-mode-map)
  (unbind-key "C-j" magit-file-section-map)
  (unbind-key "C-j" magit-hunk-section-map)

  (put 'magit-log-select-mode 'magit-log-default-arguments '("-n256" "--decorate"))
  (put 'magit-log-mode 'magit-log-default-arguments '("-n256" "--decorate"))

  (with-eval-after-load 'with-editor
    (bind-keys :map with-editor-mode-map
               ("C-c <up>" . with-editor-cancel)))

  (~add-setup-hook 'git-commit-mode
    (add-to-list '~completion-at-point-functions 'cape-emoji t))

  ;; 入力しても On <branch>: を付けるようにしてる
  (defun magit-stash-read-message ()
    (let* ((prompt (format "Stash message (default: On%s:%s): " (magit--ellipsis) (magit--ellipsis)))
           (message (read-string prompt nil nil nil)))
      (format "On %s: %s"
              (or (magit-get-current-branch) "(no branch)")
              (if (not (string= (s-trim message) ""))
                  message
                (magit-rev-format "%h %s")))))
  )

(use-package magit-section
  :after (magit))

(defun my:git-dirties ()
  (-filter
   (lambda (x) (not (string= x "")))
   (with-temp-buffer
     (projectile-run-shell-command-in-root "git status --short --untracked-files=no" (current-buffer))
     (split-string (buffer-string) "\n"))))

(defun my:git-dirty-p ()
  (> (length (my:git-dirties)) 0))

(cl-defun my:git-stash-pop-for (branch &key first)
  (let* ((stashes (mapcar (lambda (c)
                            (pcase-let ((`(,rev ,msg) (split-string c "\0")))
                              (cons msg rev)))
                          (magit-list-stashes "%gd%x00%s")))
         (stashes (if branch
                      (-filter (lambda (x)
                                 (s-starts-with? (format "On %s: " branch) (car x)))
                               stashes)
                    stashes))
         (choices (mapcar 'car stashes))
         (choice (when choices
                   (if first
                       (nth 0 choices)
                     (magit-completing-read "Pop stash: " choices nil t nil nil)))))
    (when choice
      (magit-stash-pop (assoc-default choice stashes)))))

(defun my:git-pull (remote &optional branch)
  (let ((cmd (format "git pull --rebase %s%s" remote (if branch (concat " " branch) ""))))
    (message "%s ..." cmd)
    (projectile-run-shell-command-in-root cmd)))

(defun my:git-pull-current-branch (remote)
  (interactive (list (if current-prefix-arg (magit-read-remote "Remote: ") "origin")))
  (let ((curr-branch (magit-get-current-branch))
        (stashed (when (my:git-dirty-p)
                   (call-interactively 'magit-stash-both)
                   t)))
    (my:git-pull remote)
    (when stashed (my:git-stash-pop-for curr-branch :first t))))

(defun my:git-fetch (remote)
  (interactive (list (if current-prefix-arg (magit-read-remote "Remote: ") "--all")))
  (let ((cmd (format "git fetch --prune %s" remote)))
    (message "%s ..." cmd)
    (projectile-run-shell-command-in-root cmd)))

(defun my:git-merge (branch remote)
  (interactive (list (magit-read-branch "Merge: ")
                     (if current-prefix-arg (magit-read-remote "Remote: ") "origin")))
  (let ((curr-branch (magit-get-current-branch))
        (stashed (when (my:git-dirty-p)
                   (call-interactively 'magit-stash-both)
                   t)))
    (magit-call-git "checkout" branch)
    (my:git-pull-current-branch remote)
    (magit-call-git "checkout" curr-branch)
    (magit-call-git "merge" (magit-merge-arguments) branch)
    (when stashed (my:git-stash-pop-for curr-branch :first t))))

(defun my:git-checkout (branch)
  (interactive (list (magit-read-branch "Checkout: ")))
  (when (my:git-dirty-p) (call-interactively 'magit-stash-both))
  (magit-call-git "checkout" (magit-branch-arguments) branch)
  (~persp-switch-to-current-branch)
  (my:git-stash-pop-for (magit-get-current-branch)))

(defun my:git-new-branch (branch start-point)
  (interactive (magit-branch-read-args "Create and checkout branch"))
  (when (my:git-dirty-p) (call-interactively 'magit-stash-both))
  (magit-call-git "checkout" (magit-branch-arguments) "-b" branch start-point)
  (~persp-switch-to-current-branch))

(defun my:git-stash-pop ()
  (interactive)
  (let ((branch (if current-prefix-arg nil (magit-get-current-branch))))
    (my:git-stash-pop-for branch)))


(use-package git-gutter
  :defer t
  :config
  (custom-set-variables
   '(git-gutter:lighter " GG")
   '(git-gutter:hide-gutter t)
   '(git-gutter:diff-option "-w")))


(use-package ghub :defer t)


(use-package github-browse-file
  :defer t
  :config
  (setq github-browse-file-show-line-at-point t))


;; e2wm
(use-package e2wm-vcs
  :defer t
  :config
  (setq e2wm:c-magit-recipe
        '(| (:left-size-ratio 0.3)
            (- (:upper-size-ratio 0.6)
               status branches)
            (| (:left-size-ratio 0.2)
               (- (:upper-size-ratio 0.5)
                  logs main)
               (| (:left-size-ratio 0.1)
                  diff sub))))

  (setq e2wm:c-magit-winfo
        '((:name status   :plugin magit-status)
          (:name branches :plugin magit-branches)
          (:name logs     :plugin magit-logs)
          (:name main)
          (:name diff     :buffer "*magit-diff*" :default-hide t)
          (:name sub      :buffer nil :default-hide t)))

  (e2wm:pst-class-register
   (make-e2wm:$pst-class
    :name   'magit
    :extend 'base
    :title  "Magit"
    :init   'e2wm:dp-magit-init
    :main   'status
    :start  'e2wm:dp-magit-start
    :switch 'e2wm:dp-magit-switch
    :popup  'e2wm:dp-magit-popup
    :leave  'e2wm:dp-magit-leave
    :keymap 'e2wm:dp-magit-minor-mode-map))

  ;; use only buffer that's shown on main
  (defun e2wm:def-plugin-vcs-with-window (topdir-func body-func na-buffer-func)
    (let* ((buf (wlf:get-buffer wm 'main))
           (file (buffer-file-name buf))
           (dir (or (and file (file-name-directory file))
                    (with-current-buffer buf default-directory)))
           (topdir (and dir (funcall topdir-func dir))))
      (e2wm:with-advice
       (cond
        (topdir
         (with-selected-window (wlf:get-window wm (wlf:window-name winfo))
           (with-current-buffer buf
             (funcall body-func dir topdir)
             (goto-char (point-min)))
           (wlf:set-buffer wm (wlf:window-name winfo)
                           (window-buffer (selected-window)))))
        (t
         (wlf:set-buffer wm (wlf:window-name winfo)
                         (funcall na-buffer-func)))))))

  ;; detect the window to show using not only e2wm:vcs-select-if-plugin but also others
  (defun e2wm:dp-magit-switch (buf)
    (e2wm:message "#DP MAGIT switch : %s" buf)
    (cond ((e2wm:history-recordable-p buf)
           (e2wm:with-advice
            (e2wm:pst-buffer-set 'main buf t t)))
          (t
           (or (e2wm:vcs-select-if-plugin buf)
               (let ((not-minibufp (= 0 (minibuffer-depth))))
                 (e2wm:with-advice
                  (e2wm:pst-buffer-set 'sub buf t not-minibufp)))))))

  ;; detect a commit buffer without magit-commit-buffer-name
  (defun e2wm:dp-magit-popup (buf)
    (let ((cb (current-buffer)))
      (e2wm:message "#DP MAGIT popup : %s (current %s / backup %s)"
                    buf cb e2wm:override-window-cfg-backup))
    (unless (e2wm:vcs-select-if-plugin buf)
      (let ((buf-name (buffer-name buf))
            (buf-file-name (ignore-errors
                             (file-name-nondirectory (buffer-file-name buf))))
            (wm (e2wm:pst-get-wm))
            (not-minibufp (= 0 (minibuffer-depth))))
        (e2wm:with-advice
         (cond
          ((equal buf-file-name "COMMIT_EDITMSG")
           ;; displaying commit objects in the main window
           (e2wm:pst-buffer-set 'main buf t nil))
          ((string= (buffer-name buf) "*magit-diff*")
           ;; displaying diff buffer in the diff window
           (e2wm:pst-buffer-set 'diff buf t t))
          ((string-match "^\\*magit: .*\\*$" buf-name)
           ;; displaying status object in the status window
           (e2wm:pst-buffer-set 'status buf t t))
          ((e2wm:history-recordable-p buf)
           ;; displaying recordable buffer in the main window
           (e2wm:pst-buffer-set 'main buf t t))
          (t
           ;; displaying other objects in the sub window
           (e2wm:pst-buffer-set 'sub buf t not-minibufp)))))))

  ;; use e2wm:def-plugin-vcs-na-buffer unless topdir found
  (defun e2wm:def-plugin-magit-status (frame wm winfo)
    (e2wm:def-plugin-vcs-with-window
     'magit-toplevel
     (lambda (dir topdir)
       (magit-status topdir))
     (lambda () (e2wm:def-plugin-vcs-na-buffer "Git N/A"))))

  ;; select function to show branches
  (defun e2wm:def-plugin-magit-branches (frame wm winfo)
    (e2wm:def-plugin-vcs-with-window
     'magit-toplevel
     (lambda (dir topdir)
       (dolist (f '(magit-show-branches magit-branch-manager magit-show-refs-head))
         (when (fboundp f)
           (funcall f))))
     (lambda () (e2wm:def-plugin-vcs-na-buffer "Git N/A"))))

  ;; use magit-log-section-arguments for performance
  (defun e2wm:def-plugin-magit-logs (frame wm winfo)
    (e2wm:def-plugin-vcs-with-window
     'magit-toplevel
     (lambda (dir topdir)
       (apply 'magit-log-current (cons (magit-log-read-revs t) (magit-log-arguments))))
     (lambda () (e2wm:def-plugin-vcs-na-buffer "Git N/A"))))

  ;; For p-r

  ;; magit では、パースペクティブ切替後に再度プラグインのupdateを実行しないと、
  ;; diffやbranchなどのmagitコマンド実行時に、以前のリポジトリに切り替わってしまったりする
  (advice-add 'e2wm:dp-magit :after '~e2wm:dp-magit-after)
  (defun ~e2wm:dp-magit-after ()
    (let* ((instance (e2wm:pst-get-instance))
           (wm (e2wm:$pst-wm instance)))
      (e2wm:pst-method-call e2wm:$pst-class-update instance wm))
    )
  )


(defun ~git-grep-file-diff-history ()
  (interactive)
  (let* ((branches (loop for b in (split-string (shell-command-to-string "git branch") "\n")
                         for b = (replace-regexp-in-string "\\* " "" b)
                         for b = (s-trim b)
                         if (not (string= b ""))
                         collect b))
         (branch (completing-read "Branch: " branches nil t nil '()))
         (file (expand-file-name (completing-read "File: " (projectile-current-project-files) nil t nil '())
                                 (projectile-project-root)))
         (query (read-string "Queyr: "))
         (commits (loop for loginfo in (split-string (shell-command-to-string
                                                      (format "git log --branches=%s %s" branch file))
                                                     "^commit +")
                        for commit = nil
                        do (loop for e in (split-string loginfo "\n")
                                 do (cond ((not (plist-get commit :id))
                                           (plist-put commit :id e))
                                          ((string-match "\\`Author: +\(.+\)\\'" e)
                                           (plist-put commit :author (match-string-no-properties 1 e)))
                                          ((string-match "\\`Date: +\(.+\)\\'" e)
                                           (plist-put commit :date (match-string-no-properties 1 e)))
                                          (t
                                           (plist-put commit :msg
                                                      (concat (or (plist-get commit :msg) "")
                                                              (s-trim e)
                                                              "\n")))))
                        if commit
                        collect commit))
         (matched-commits (loop for commit in commits
                                for diff = (shell-command-to-string
                                            (format "git diff %s^ %s %s:%s"
                                                    (plist-get commit :id)
                                                    (plist-get commit :id)
                                                    branch
                                                    file))
                                if (string-match query diff)
                                collect (progn
                                          (plist-get commit :diff diff)
                                          commit))))))
