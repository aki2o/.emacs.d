;; スタートアップ時のメッセージを抑制
(setq inhibit-startup-message t)

;; 履歴を次回起動時も有効にする
(savehist-mode 1)

;; 対応する括弧を光らせる
(show-paren-mode 1)
;; (setq show-paren-mode t)
;; (setq blink-matching-paren t)
;; (setq show-paren-style 'expression)
;; (set-face-background 'show-paren-match-face "gray10")
;; (set-face-foreground 'show-paren-match-face "SkyBlue")

;; utf-8優先
(prefer-coding-system 'utf-8)

;; インデントはスペースで
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; 色つける
(global-font-lock-mode t)
;; (require 'font-lock)

;; 現在行のハイライト
(bind-key* "<f3>" 'global-hl-line-mode)
(use-package hl-line
  :config
  (set-face-background 'hl-line "gray20"))

;; Shift + 矢印キーで範囲選択
;; 選択範囲に色をつける
(setq-default transient-mark-mode t)
(transient-mark-mode t)

;; 一行ずつスクロール
(setq scroll-step 1)

;; 画面をページアップ、ページダウンするときにカーソルを固定
(setq scroll-preserve-screen-position t)

;; リージョンをC-hで削除
(delete-selection-mode 1)

;; GCを減らす （デフォルトの10倍）
(setq gc-cons-threshold (* 10 gc-cons-threshold))

;; ログ記録行数
(setq message-log-max 10000)

;; ミニバッファを再帰的に呼び出せるようにする
(setq enable-recursive-minibuffers t)

;; ダイアログボックスを使わない
(setq use-dialog-box nil)
(defalias 'message-box 'message)

;; 履歴保存数
(setq history-length 1000)

;; キー入力をエコーエリアに早く表示する
(setq echo-keystrokes 0.1)

;; メニューバーとツールバーoff
(tool-bar-mode 0)
(menu-bar-mode 0)

;; スクロールバー非表示
(scroll-bar-mode 0)

;; フリンジ(左右の折り返し表示領域)はいらない
(fringe-mode 1)

;; 通常時、折り返さない
(setq truncate-lines t)
;; 縦分割で行は折り返さない
(setq truncate-partial-width-windows t)

;; 行数、列数を表示
(line-number-mode t)
(column-number-mode t)
(bind-key* "<f12>" 'linum-mode)

;; ウィンドウを縦に分割するかどうかの閾値
; (custom-set-variables '(split-width-threshold 44))

;; 反対側のウィンドウにいけるように
(setq windmove-wrap-around t)

;; ミニバッファで入力を取り消しても履歴を残す
(defadvice abort-recursive-edit (before minibuffer-save activate)
  (when (eq (selected-window) (active-minibuffer-window))
    (add-to-history minibuffer-history-variable (minibuffer-contents))))

;; yでyesの代わりとする
(defalias 'yes-or-no-p 'y-or-n-p)

;; フレーム
(set-frame-parameter (selected-frame) 'alpha '(85 . 60)) ;; 透明度
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(set-frame-parameter (selected-frame) 'cursor-type 'hbar)
(set-frame-parameter (selected-frame) 'cursor-color "white")

;; 現在位置のファイルやURLを開く
;(ffap-bindings)

; (highlight-regexp)

;; ヴィジュアルベル無効
(setq visible-bell nil)

;; ビープ音も無効
(setq ring-bell-function '(lambda ()))

;; バックアップしない
(setq make-backup-files nil)

;; 自動保存したファイルを削除する
(setq delete-auto-save-files t)

;; 自動セーブしない
;(setq auto-save-default nil)

;;; カーソル
;; (set-cursor-type 'hbar)
;; (set-cursor-color "white")
;(blink-cursor-mode 0)

;;; マウスを消す設定
(setq w32-hide-mouse-on-key t)
(setq w32-hide-mouse-timeout 5000)

;; アクティブでないバッファではカーソルを出さない
(setq cursor-in-non-selected-windows nil)

;; ファイル内のカーソル位置を記憶する
(use-package saveplace
  :init
  (setq save-place t))

;; ミニバッファのサイズ変更するか
(setq resize-mini-windows t)

;; lsp とか使うと、デフォルトは小さすぎて、動作が重くなるので、大きくする必要があるらしい
(setq gc-cons-threshold 200000000)

;; https://emacs-lsp.github.io/lsp-mode/page/performance/
(setq read-process-output-max (* 1024 1024)) ; 1MB

(setq register-preview-delay 0.5)
