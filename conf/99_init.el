(setq inhibit-startup-message t) ;; スタートアップ時のメッセージを抑制
(setq visible-bell nil) ;; ヴィジュアルベル無効
(setq ring-bell-function '(lambda ())) ;; ビープ音も無効
(setq use-dialog-box nil) ;; ダイアログボックスを使わない

(setq message-log-max 10000) ;; ログ記録行数
(setq history-length 1000) ;; 履歴保存数
(setq echo-keystrokes 0.1) ;; キー入力をエコーエリアに早く表示する
(setq gc-cons-threshold 200000000) ;; lsp とか使うと、デフォルトは小さすぎて、動作が重くなるので、大きくする必要があるらしい
(setq read-process-output-max (* 1024 1024)) ;; https://emacs-lsp.github.io/lsp-mode/page/performance/
(setq register-preview-delay 0.5)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(savehist-mode 1) ;; 履歴を次回起動時も有効にする
(delete-selection-mode 1) ;; リージョンをC-hで削除

(defalias 'message-box 'message)
(defalias 'yes-or-no-p 'y-or-n-p)

;; ファイル内のカーソル位置を記憶する
(use-package saveplace
  :init
  (setq save-place t))
