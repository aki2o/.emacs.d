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
(use-package dsvn
  :defer t
  :commands (svn-status svn-update))


;; Git
(setenv "GIT_PAGER" "")


(use-package magit
  :defer t
  :init
  (use-package ghub :defer t)
  :config
  (unbind-key "j" magit-status-mode-map)
  (unbind-key "C-j" magit-file-section-map)
  (unbind-key "C-j" magit-hunk-section-map))


(use-package magit-popup
  :after (magit)
  :defer t)


(use-package git-gutter
  :defer t
  :config
  (custom-set-variables
   '(git-gutter:lighter " GG")
   '(git-gutter:hide-gutter t)
   '(git-gutter:diff-option "-w")))


(use-package github-browse-file
  :defer t
  :config
  (setq github-browse-file-show-line-at-point t))


(use-package gitignore-mode
  :defer t)


;; e2wm
(use-package e2wm-vcs
  :straight e2wm
  :defer t
  :config
  ;; svn
  (setq e2wm:c-svn-focus-buffer-regexp ".")

  ;; p-r
  
  (setq e2wm:c-svn-recipe
        '(| (:left-size-ratio 0.3)
            status
            (| (:left-size-ratio 0.2)
               (- (:upper-size-ratio 0.5)
                  logs main)
               (| (:left-size-ratio 0.1)
                  diff sub))))

  (setq e2wm:c-svn-winfo
        '((:name status :plugin svn-status)
          (:name logs   :plugin svn-logs)
          (:name main)
          (:name diff   :buffer "*svn output*" :default-hide t)
          (:name sub    :buffer nil :default-hide t)))

  (e2wm:pst-class-register
   (make-e2wm:$pst-class
    :name   'svn
    :extend 'base
    :title  "Svn"
    :init   'e2wm:dp-svn-init
    :main   'status
    :switch 'e2wm:dp-svn-switch
    :popup  'e2wm:dp-svn-popup
    :leave  'e2wm:dp-svn-leave
    :keymap 'e2wm:dp-svn-minor-mode-map))

  (defun e2wm:dp-svn-init ()
    (let* ((svn-wm
            (wlf:no-layout e2wm:c-svn-recipe e2wm:c-svn-winfo))
           (buf (or e2wm:prev-selected-buffer
                    (e2wm:history-get-main-buffer))))
      (wlf:set-buffer svn-wm 'main buf)
      (wlf:select svn-wm 'status)
      svn-wm))

  (defun e2wm:dp-svn-switch (buf)
    (e2wm:message "#DP SVN switch : %s" buf)
    (cond ((e2wm:history-recordable-p buf)
           (e2wm:with-advice
            (e2wm:pst-buffer-set 'main buf t t)))
          ((string= (buffer-name buf) "*log-edit-files*")
           nil)
          (t
           (or (e2wm:vcs-select-if-plugin buf)
               (e2wm:dp-svn-popup-sub buf)))))

  (defun e2wm:dp-svn-popup (buf)
    (let ((cb (current-buffer)))
      (e2wm:message "#DP SVN popup : %s (current %s / backup %s)"
                    buf cb e2wm:override-window-cfg-backup))
    (cond ((e2wm:history-recordable-p buf)
           (e2wm:with-advice
            (e2wm:pst-buffer-set 'main buf t t)))
          ((string= (buffer-name buf) "*svn output*")
           (e2wm:with-advice
            (e2wm:pst-buffer-set 'diff buf t t)))
          (t
           (e2wm:dp-svn-popup-sub buf))))

  (defun e2wm:dp-svn-popup-sub (buf)
    (let* ((wm (e2wm:pst-get-wm))
           (bufname (buffer-name buf))
           (focus-set (and (= 0 (minibuffer-depth))
                           (string-match e2wm:c-svn-focus-buffer-regexp bufname))))
      (e2wm:with-advice
       (e2wm:pst-buffer-set 'sub buf t focus-set))))


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
  
  ;; use magit-status-internal as substitute for magit-status

  (defun e2wm:def-plugin-magit-status (frame wm winfo)
    (e2wm:def-plugin-vcs-with-window
     (e2wm:magit-top-dir-function)
     (lambda (dir topdir)
       (loop for f in '(magit-status-internal magit-status)
             if (fboundp f)
             return (funcall f (file-name-as-directory dir))))
     (lambda () (e2wm:history-get-main-buffer))))

  ;; select function to show branches
  
  (defun e2wm:def-plugin-magit-branches (frame wm winfo)
    (e2wm:def-plugin-vcs-with-window
     (e2wm:magit-top-dir-function)
     (lambda (dir topdir)
       (dolist (f '(magit-show-branches magit-branch-manager magit-show-refs-head))
         (when (fboundp f)
           (funcall f))))
     (lambda () (e2wm:def-plugin-vcs-na-buffer "Git N/A"))))

  ;; use magit-log-section-arguments for performance
  
  (defun e2wm:def-plugin-magit-logs (frame wm winfo)
    (e2wm:def-plugin-vcs-with-window
     (e2wm:magit-top-dir-function)
     (lambda (dir topdir)
       (magit-log nil (or (ignore-errors magit-log-section-arguments)
                          '("-n256" "--decorate"))))
     (lambda () (e2wm:def-plugin-vcs-na-buffer "Git N/A"))))

  ;; use magit-toplevel not magit-get-top-dir

  (defun e2wm:magit-top-dir-function ()
    (loop for f in '(magit-get-top-dir magit-toplevel)
          if (fboundp f)
          return f)))


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
