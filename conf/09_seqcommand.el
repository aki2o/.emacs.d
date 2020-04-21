(bundle sequential-command)
(bundle emacswiki:sequential-command-config)
(use-package sequential-command-config
  :config
  ;; デフォルト設定はしないで独自に定義
  ;; (sequential-command-setup-keys)

  ;; インデント頭と行頭を行ったり来たり
  (define-sequential-command seq-beginning-of-line
    beginning-of-line back-to-indentation seq-return)

  ;; 画面中央、画面最上部、画面最下部に移動するようにする
  (define-sequential-command seq-recenter
    recenter ~move-to-top-of-window ~move-to-down-of-window seq-return)
  
  (defun ~move-to-top-of-window ()
    (interactive)
    (recenter 0))

  (defun ~move-to-down-of-window ()
    (interactive)
    (recenter -1))

  ;; バッファ全体の見出し絞り込み
  (define-sequential-command seq-yaol-heads
    yaol-fold-in-popular-level-heads
    yaol-fold-in-popular-heads
    yaol-fold-in-all-heads)

  ;; 現在見出し配下の見出し絞り込み
  (define-sequential-command seq-yaol-current-heads
    yaol-fold-in-child-heads-without-body
    yaol-fold-in-child-heads
    yaol-fold-in-popular-descendant-heads
    yaol-fold-in-all-descendant-heads))
