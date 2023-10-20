(defun ~vc-minor-mode ()
  (interactive)
  (let ((file (expand-file-name (buffer-file-name))))
    (cond ((string-match ~tramp-path-regexp file)
           (message "Can't start mode : This is a tramp buffer"))
          ((~file-exist-in-tree-p file ".git" t)
           (git-gutter-mode))
          (t
           (message "Can't start mode : Not found available mode for this buffer")))))


;; SVN
(bundle dsvn)
(use-package dsvn
  :defer t
  :commands (svn-status svn-update))


;; Git
(setenv "GIT_PAGER" "")
(bundle git-modes)
(use-package git-modes
  :defer t)


(bundle magit)
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

  ;; Emacs29以降だった
  ;; (~add-setup-hook-after-load 'cape 'git-commit-mode
  ;;   (make-local-variable 'completion-at-point-functions)
  ;;   (add-to-list 'completion-at-point-functions 'cape-emoji t))
  )

(bundle magit-section)
(use-package magit-section
  :after (magit))


(bundle git-gutter)
(use-package git-gutter
  :defer t
  :config
  (custom-set-variables
   '(git-gutter:lighter " GG")
   '(git-gutter:hide-gutter t)
   '(git-gutter:diff-option "-w")))


(bundle ghub)
(use-package ghub :defer t)


(bundle github-browse-file)
(use-package github-browse-file
  :defer t
  :config
  (setq github-browse-file-show-line-at-point t))


;; e2wm
(bundle e2wm)
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
    (let* ((buf (wlf:get-buffer (e2wm:pst-get-wm) 'main))
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

  ;; magit では、なぜか再度 e2wm:pst-update-windows を実行しないと、
  ;; diffやbranchなどのmagitコマンド実行時に、以前のリポジトリに切り替わってしまったりする
  (advice-add 'e2wm:dp-magit :after '~e2wm:dp-magit-after)
  (defun ~e2wm:dp-magit-after ()
    (e2wm:pst-update-windows))
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
