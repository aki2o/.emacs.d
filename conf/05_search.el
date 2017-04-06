(bind-keys :map isearch-mode-map
           ("C-r"   . nil)
           ("C-S-s" . isearch-repeat-backward)
           ("C-h"   . isearch-del-char)
           ("C-l"   . isearch-yank-char)
           ("C-n"   . isearch-ring-advance)
           ("C-p"   . isearch-ring-retreat))

;; リージョン選択されたテキストが検索文字列としてあらかじめ設定された状態でisearch
(defadvice isearch-mode (around
                         isearch-mode-default-string
                         (forward &optional regexp op-fun recursive-edit word-p)
                         activate)
  (if (and transient-mark-mode
           mark-active
           (not (eq (mark) (point))))
      (progn
        (isearch-update-ring (buffer-substring-no-properties (mark) (point)))
        (deactivate-mark)
        ad-do-it
        (if (not forward)
            (isearch-repeat-backward)
          (goto-char (mark))
          (isearch-repeat-forward)))
    ad-do-it))

(cond ((~is-mac)
       ;; [migemo]isearch のとき IME を英数モードにする
       (add-hook 'isearch-mode-hook 'mac-change-language-to-us t))
      ((~is-windows)
       ;; isearch 時はオフに
       (add-hook 'isearch-mode-hook 'ibus-disable t)))


(bundle highlight-symbol)
(use-package highlight-symbol
  :bind* (("<f3>" . highlight-symbol-mode))
  :config
  (defun highlight-symbol-count ()))


