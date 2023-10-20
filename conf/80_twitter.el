;; (bundle twittering-mode)
;; (use-package twittering-mode
;;   :defer t
;;   :custom ((twittering-auth-method 'oauth)
;;            ;; パスワード認証
;;            (twittering-use-master-password t)
;;            ;; (twittering-private-info-file "~/.twittering-mode/twittering-mode.gpg")
;;            ;; 表示書式
;;            ;; (twittering-status-format "%i @%s %S %p: n %T  [%@]%r %R %f%Ln -------------------------------------------")
;;            ;; アイコン表示
;;            (twittering-icon-mode t)
;;            ;; アイコンサイズを変更する。imagemagickが必要
;;            (twittering-convert-fix-size (when (executable-find "convert") 36))
;;            ;; 更新の頻度（秒）
;;            (twittering-timer-interval 30)
;;            ;; ツイート取得数
;;            (twittering-number-of-tweets-on-retrieval 100))

;;   :config
;;   (bind-keys :map twittering-mode-map
;;              ("o" . ~twittering-browse-url)))

;; (defun ~~twittering-browse-url ()
;;   (interactive)
;;   (twittering-goto-next-uri)
;;   (execute-kbd-macro (kbd "C-m")))
