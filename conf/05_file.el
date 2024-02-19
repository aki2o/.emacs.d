(setq make-backup-files nil) ;; バックアップしない
(setq delete-auto-save-files t) ;; 自動保存したファイルを削除する
;; (setq auto-save-default nil) ;; 自動セーブしない

;; 様子見
;; (setq confirm-nonexistent-file-or-buffer nil)

(with-eval-after-load 'tramp
  (setq tramp-default-method "scpx")

  ;; 多段接続
  ;; Ex.) localhost -> ssh -> foo@host1 -> ssh -> bar@host2
  ;; (add-to-list 'tramp-default-proxies-alist '("host2" "bar" "/ssh:foo@host1"))
  ;;
  ;; リモートをrootで操作する時、まずssh_configで設定されたユーザまたはローカルユーザで接続する
  (add-to-list 'tramp-default-proxies-alist '(nil "\\`root\\'" "/ssh:%h:"))
  ;; 但し、直接rootログインできるホストやローカルホストは除く
  (dolist (h `("localhost" ,(regexp-quote (system-name))))
    (add-to-list 'tramp-default-proxies-alist `(,h nil nil)))

  ;; trampが最初にauth-source-searchを呼び出すと固まってしまうため、その前に実行しておく
  (with-eval-after-load 'auth-source
    (auth-source-search :user t :host "dummy" :port "dummy"))
  )


(use-package recentf-ext)
(recentf-mode t)
(setq recentf-max-saved-items 3000)
(setq recentf-exclude '("/TAGS" "/ETAGS" "/tmp/"))

(defun ~recentf-push-buffers-in-frame ()
  (walk-windows
   (lambda (win)
     (let ((bfn (buffer-local-value 'buffer-file-name (window-buffer win))))
       (and bfn (recentf-add-file bfn))))))

(add-to-list 'window-configuration-change-hook '~recentf-push-buffers-in-frame)

(defun ~recentf-add-dired-directory ()
  (when (and (stringp dired-directory)
             (equal "" (file-name-nondirectory dired-directory)))
    (recentf-add-file dired-directory)))

(add-hook 'dired-mode-hook '~recentf-add-dired-directory)
