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
  (define-sequential-command seq-outline-headings
    ~outline-popular-level-headings ~outline-current-level-headings ~outline-all-headings)

  ;; 現在見出し配下の見出し絞り込み
  (define-sequential-command seq-outline-current-headings
    ~outline-current-child-headings ~outline-current-all-headings ~outline-current-all-headings-with-body ~outline-current-all-body))
