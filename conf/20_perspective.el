(use-package persp-mode
  :defer t
  :commands (persp-switch-to-buffer)
  :custom ((persp-keymap-prefix (kbd "C-x p"))
           (persp-save-dir (expand-file-name ".persp-confs/" user-emacs-directory))
           (persp-switch-to-added-buffer nil)                           ; バッファ追加時にそのバッファへは切り替えさせない
           (persp-auto-save-opt 0)                                      ; 自動でファイルへの保存はさせない
           (persp-auto-resume-time 0)                                   ; 起動時に自動で復元はさせない
           (persp-restore-window-conf-method (lambda (&rest args) nil)) ; 切り替え時には何もしない
           (persp-common-buffer-filter-functions '(~persp-common-buffer-filter)))
  :config
  (persp-mode 1)

  (setq persp-interactive-completion-function '~persp-interactive-completion-function)

  (define-key persp-key-map (kbd "s") '~persp-switch-to-current-branch)
  (define-key persp-key-map (kbd "S") 'persp-frame-switch)
  (define-key persp-key-map (kbd "A") '~persp-add-buffers-from)
  (define-key persp-key-map (kbd "K") '~persp-remove-all-buffers)
  (define-key persp-key-map (kbd "i") '~persp-add-git-diff-files)

  (~persp-switch-to-current-branch))
  
;; パースペクティブ選択で、関係ないhistoryのリストが出てきてウザイので無効にする
(defun ~persp-interactive-completion-function (prompt collection &optional predicate require-match initial hist default inherit-input-method)
  (let ((empty-list '()))
    (completing-read prompt collection predicate require-match nil 'empty-list default)))

;; Gitで現在のブランチのパースペクティブを自動で用意して切り替えられるようにする
(defun ~persp-switch-to-current-branch ()
  (interactive)
  (let* ((persp-name (ignore-errors (persp-name (get-current-persp))))
         (proj-name (ignore-errors
                      (file-name-nondirectory
                       (directory-file-name (projectile-project-root)))))
         (branch-name (ignore-errors (magit-get-current-branch)))
         (new-persp-name (when (and proj-name branch-name)
                           (format "%s:%s" proj-name branch-name))))
    (when (and new-persp-name
               (not (eql persp-name new-persp-name)))
      (message "Switch perspective to %s ..." new-persp-name)
      (persp-switch new-persp-name)
      (message "Switch perspective to %s done." new-persp-name))))

;; 全バッファ除去がないっぽいので定義
(defun ~persp-remove-all-buffers ()
  (interactive)
  (let ((currbuf (current-buffer)))
    (dolist (buf (persp-buffer-list))
      (when (not (eql buf currbuf))
        (persp-remove-buffer buf)))))

;; 一括追加
(defun ~persp-add-buffers-from (name)
  (interactive (list (persp-read-persp "from: " nil nil t)))
  (let* ((buffers (safe-persp-buffers (persp-get-by-name name))))
    (dolist (buf (my:filtering-read buffers :printer 'buffer-name))
      (persp-add-buffer buf))))

;; パースペクティブにブランチで編集したファイルを追加する
(defun ~persp-add-git-diff-files (branch)
  (interactive
   (list (completing-read "Base: " (magit-list-local-branch-names) nil t nil '())))
  (loop with root = (locate-dominating-file default-directory ".git")
        for f in (split-string (shell-command-to-string (format "git diff --name-only %s" branch)) "\n")
        for path = (expand-file-name f root)
        if (file-regular-p path)
        do (my:run-deferred-with path 1
             (persp-add-buffer (find-file-noselect it))
             (message "added perspective entry : %s" it))))

;; p-r

;; buffer-name が nil の場合があり、 string-prefix-p がエラーになる対処
(defun ~persp-common-buffer-filter (b)
  (or (and (buffer-name b) (string-prefix-p " " (buffer-name b)))
      (eq (buffer-local-value 'major-mode b) 'helm-major-mode)))


(use-package e2wm-perspb
  :after (e2wm))

(use-package e2wm-perspb-rails
  :after e2wm-perspb)

(use-package e2wm-perspb-ts
  :after e2wm-perspb)
