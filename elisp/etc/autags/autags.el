(require 'concurrent)

(defvar my-update-tag-semaphore (cc:semaphore-create 1))

;; バッファ毎に設定するTAGファイル拡張子名
(make-variable-buffer-local 'my-tags-mode-name)

;; 読み込むTAGファイルの拡張子を取得
(defun my-get-tags-mode-name () "get mode name of current buffer for tag info saving."
  (if (stringp my-tags-mode-name)
      my-tags-mode-name
    (replace-regexp-in-string "-mode" "" (symbol-name major-mode))))

;; カレントディレクトリのTAG情報を更新
(defun my-update-current-tag-info () "update tags info in current directory."
  (interactive)
  (deferred:$
    (deferred:process-shellc (cc:semaphore-acquire my-update-tag-semaphore)
      "mketags"
      "--suffix=" (file-name-extension (buffer-file-name (current-buffer)))
      "--language=" (my-get-tags-mode-name)
      (shell-quote-argument default-directory))
    (deferred:watch it
      (lambda ()
        (cc:semaphore-release my-update-tag-semaphore)))))

;; TAG情報削除
(defun my-clean-tag-info (mode) "clean up tags info."
  (interactive
   (list (read-string "mode name for delete tags table: " (symbol-name major-mode) nil nil)))
  (shell-command 
   (concat "mketags"
           " --clean"
           " --language=" (replace-regexp-in-string "-mode" "" mode))))

;; 特定のディレクトリ配下のTAG情報を登録
(defun my-append-tree-tag-info (dir mode suffix) "append tags info within directory."
  (interactive
   (list (read-string "directory for regist tags table: " default-directory nil nil)
         (read-string "mode name for regist tags table: " (symbol-name major-mode) nil nil)
         (read-string "suffix for regist tags table: " (file-name-extension (buffer-file-name (current-buffer))))))
  (shell-command 
   (concat "mketags"
           " --suffix=" suffix
           " --language=" (replace-regexp-in-string "-mode" "" mode)
           " --recursive " (shell-quote-argument dir))))

;; TAGファイル読み込み
(defun my-visit-tags-table () "visit tags table for me."
  (interactive)
  (when (not (file-exists-p (concat "~/ETAGS." (my-get-tags-mode-name))))
    (my-append-tree-tag-info))
  (visit-tags-table (concat "~/ETAGS." (my-get-tags-mode-name))))

;; ファイル保存と同時にカレントのTAG情報を更新
(add-hook 'after-save-hook 'my-update-current-tag-info t)

;; TAGファイルがなかったら、とりあえずカレントだけ自動登録する
(defadvice find-tag (before before-find-tag activate) "Automatically visit tags file."
  (my-visit-tags-table))

;; TAGファイルを自動更新しているため、visit-tags-tableで毎回確認応答が出てうざい対処
(defadvice yes-or-no-p (around yes-or-no-p-auto-yes-for-tag-file-read activate)
  "Automatically read tag file if it's new contents."
  (if (string-match "^Tags file .+ has changed, read new contents" (ad-get-arg 0))
      (setq ad-return-value t)
    ad-do-it))
;; (defadvice y-or-n-p (around around-y-or-n-p-auto-yes-for-tag-file-read activate)
;;   "Automatically read tag file if it's new contents."
;;   ad-do-it)
