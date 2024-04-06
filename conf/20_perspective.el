(use-package persp-mode
  :defer t
  :commands (persp-switch-to-buffer)
  :custom ((persp-keymap-prefix (kbd "C-x p"))
           (persp-save-dir (expand-file-name ".persp-confs/" user-emacs-directory))
           (persp-switch-to-added-buffer nil) ; バッファ追加時にそのバッファへは切り替えさせない
           (persp-auto-save-opt 0)            ; 自動でファイルへの保存はさせない
           (persp-auto-resume-time 0)         ; 起動時に自動で復元はさせない
           (persp-common-buffer-filter-functions '(~persp-common-buffer-filter)))
  :config
  (persp-mode 1)

  (setq persp-interactive-completion-function '~persp-interactive-completion-function)

  (define-key persp-key-map (kbd "s") '~persp-switch-to-current-branch)
  (define-key persp-key-map (kbd "S") 'persp-frame-switch)
  (define-key persp-key-map (kbd "K") '~persp-remove-all-buffers)

  (add-to-list 'persp-before-deactivate-functions '~persp-save-state t))
  
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

;; persp-auto-save-opt以外では、パースペクティブの状態保存をしないようなので、
;; パースペクティブが終了するタイミングでメモリ上に保存するようにする
(defun ~persp-save-state (frame-or-window)
  (let ((persp (cl-case frame-or-window
                 (frame  (get-frame-persp))
                 (window (get-window-persp)))))
    (when persp
      (persp-save-state persp))))

;; 全バッファ除去がないっぽいので定義
(defun ~persp-remove-all-buffers ()
  (interactive)
  (let ((currbuf (current-buffer)))
    (dolist (buf (persp-buffer-list))
      (when (not (eql buf currbuf))
        (persp-remove-buffer buf)))))

;; p-r

;; buffer-name が nil の場合があり、 string-prefix-p がエラーになる対処
(defun ~persp-common-buffer-filter (b)
  (or (and (buffer-name b) (string-prefix-p " " (buffer-name b)))
      (eq (buffer-local-value 'major-mode b) 'helm-major-mode)))


(use-package e2wm-perspb
  :after (e2wm)
  :config
  (~persp-switch-to-current-branch)

  (with-eval-after-load 'magit
    ;; ブランチを切り替えたタイミングで、パースペクティブも切り替わるようにする
    (defadvice magit-checkout (after ~persp-switch activate)
      (ignore-errors (~persp-switch-to-current-branch)))
    
    (defadvice magit-branch (after ~persp-switch activate)
      (ignore-errors (~persp-switch-to-current-branch)))

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

    (define-key persp-key-map (kbd "i") '~persp-add-git-diff-files))
  (require 'magit nil t))

(use-package e2wm-perspb-rails
  :after e2wm-perspb)

(use-package e2wm-perspb-ts
  :after e2wm-perspb)
