;; NOTE: 環境変数を参照する必要がある設定がありそうなので 01 にしていたが、 copilot の起動に失敗するようになってしまい（ node が見つからない ）、原因わかってないが 99 にしたら解消した
(when (memq window-system '(mac ns))
  (require 'exec-path-from-shell)
  (exec-path-from-shell-initialize))
