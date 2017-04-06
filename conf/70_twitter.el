(bundle twittering-mode)
(use-package twittering-mode
  :defer t
  
  :init

  (setq twittering-auth-method 'oauth)

  ;; パスワード認証
  (setq twittering-use-master-password t)
  ;; (setq twittering-private-info-file "~/.twittering-mode/twittering-mode.gpg")

  ;; 表示書式
  ;; (setq twittering-status-format "%i @%s %S %p: n %T  [%@]%r %R %f%Ln -------------------------------------------")

  ;; アイコン表示
  (setq twittering-icon-mode t)

  ;; アイコンサイズを変更する。imagemagickが必要
  (when (executable-find "convert")
    (setq twittering-convert-fix-size 36))

  ;; 更新の頻度（秒）
  (setq twittering-timer-interval 30)

  ;; ツイート取得数
  (setq twittering-number-of-tweets-on-retrieval 100)

  :config
  
  ;; ;; o で次のURLをブラウザでオープン
  ;; (add-hook 'twittering-mode-hook
  ;;           (lambda ()
  ;;             (local-set-key (kbd "o")
  ;;                (lambda ()
  ;;                  (interactive)
  ;;                  (twittering-goto-next-uri)
  ;;                  (execute-kbd-macro (kbd "C-m"))
  ;;                  )))
  ;;           t)

  )

