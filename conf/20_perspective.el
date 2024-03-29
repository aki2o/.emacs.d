(bundle persp-mode)
(use-package persp-mode
  :defer t
  :commands (persp-switch-to-buffer)
  :init
  (setq persp-keymap-prefix (kbd "C-x p"))
  (setq persp-save-dir (expand-file-name ".persp-confs/" user-emacs-directory))
  
  (setq persp-switch-to-added-buffer nil) ; バッファ追加時にそのバッファへは切り替えさせない
  (setq persp-auto-save-opt 0)            ; 自動でファイルへの保存はさせない
  (setq persp-auto-resume-time 0)         ; 起動時に自動で復元はさせない

  ;; p-r

  ;; buffer-name が nil の場合があり、 string-prefix-p がエラーになる対処
  (setq persp-common-buffer-filter-functions
        (list #'(lambda (b) (or (and (buffer-name b) (string-prefix-p " " (buffer-name b)))
                                (eq (buffer-local-value 'major-mode b) 'helm-major-mode)))))
  
  (persp-mode 1)

  :config
  ;; パースペクティブ選択で、関係ないhistoryのリストが出てきてウザイので無効にする
  (defun ~persp-interactive-completion-function (prompt collection &optional predicate require-match initial hist default inherit-input-method)
    (let ((empty-list '()))
      (completing-read prompt collection predicate require-match nil 'empty-list default)))

  (setq persp-interactive-completion-function '~persp-interactive-completion-function)

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

  (define-key persp-key-map (kbd "s") '~persp-switch-to-current-branch)
  (define-key persp-key-map (kbd "S") 'persp-frame-switch)

  ;; persp-auto-save-opt以外では、パースペクティブの状態保存をしないようなので、
  ;; パースペクティブが終了するタイミングでメモリ上に保存するようにする
  (defun ~persp-save-state (frame-or-window)
    (let ((persp (cl-case frame-or-window
                   (frame  (get-frame-persp))
                   (window (get-window-persp)))))
      (when persp
        (persp-save-state persp))))
  
  (add-to-list 'persp-before-deactivate-functions '~persp-save-state t)

  ;; 全バッファ除去がないっぽいので定義
  (defun ~persp-remove-all-buffers ()
    (interactive)
    (let ((currbuf (current-buffer)))
      (dolist (buf (persp-buffer-list))
        (when (not (eql buf currbuf))
          (persp-remove-buffer buf)))))

  (define-key persp-key-map (kbd "K") '~persp-remove-all-buffers)
  
  
  (with-eval-after-load 'e2wm
    ;; パースペクティブに対応したpstに切り替わるようにする
    (defun ~persp-save-e2wm-pst (frame-or-window)
      (let ((persp (cl-case frame-or-window
                     (frame  (get-frame-persp))
                     (window (get-window-persp)))))
        (set-persp-parameter 'e2wm-pst (e2wm:pst-get-instance) persp)))
    
    (add-to-list 'persp-before-deactivate-functions '~persp-save-e2wm-pst t)

    (defun ~persp-load-e2wm-pst (frame-or-window)
      (let* ((persp (cl-case frame-or-window
                      (frame  (get-frame-persp))
                      (window (get-window-persp))))
             (pst (persp-parameter 'e2wm-pst persp)))
        (when (e2wm:$pst-p pst)
          (e2wm:pst-change (e2wm:$pst-name pst)))))

    (add-to-list 'persp-activated-functions '~persp-load-e2wm-pst t))
  

  (with-eval-after-load 'helm-buffers
    ;; バッファリストにhelmを使う
    (defclass ~persp-helm-source-buffers (helm-source-buffers)
      ((buffer-list
        :initarg :buffer-list
        :initform #'~persp-helm-buffer-list
        :custom function
        :documentation
        "  A function with no arguments to create buffer list.")))

    (defvar ~persp-helm-source-buffers-list nil)
    
    (defun ~persp-helm-buffer-list ()
      (delete-if 'persp-buffer-filtered-out-p
                 (mapcar 'buffer-name
                         (remove-if-not 'buffer-live-p (persp-buffer-list-restricted)))))

    (defun persp-switch-to-buffer (buffer-or-name &optional norecord force-same-window)
      (interactive (list
                    (helm :sources (list (or ~persp-helm-source-buffers-list
                                             (setq ~persp-helm-source-buffers-list
                                                   (helm-make-source "Buffers" '~persp-helm-source-buffers))))
                          :buffer "*persp buffers*"
                          :keymap helm-buffer-map
                          :truncate-lines t)))
      (switch-to-buffer buffer-or-name norecord force-same-window))))


(bundle e2wm-perspb :type github :pkgname "aki2o/e2wm-perspb")
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
      (loop with root = (projectile-project-root)
            for f in (split-string (shell-command-to-string (format "git diff --name-only %s" branch)) "\n")
            for path = (concat root f)
            if (file-regular-p path)
            do (progn
                 (persp-add-buffer (find-file-noselect path))
                 (message "added perspective entry : %s" f))))

    (define-key persp-key-map (kbd "i") '~persp-add-git-diff-files))
  (require 'magit nil t))


(use-package e2wm-perspb-rails
  :after e2wm-perspb)
