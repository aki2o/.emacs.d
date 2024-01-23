(use-package sequential-command)

;; インデント頭と行頭を行ったり来たり
(define-sequential-command seq-beginning-of-line
  beginning-of-line back-to-indentation seq-return)

;; 画面中央、画面最上部、画面最下部に移動するようにする
(define-sequential-command seq-recenter
  recenter ~scroll-to-top-of-window ~scroll-to-down-of-window seq-return)
