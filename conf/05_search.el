(bind-keys :map isearch-mode-map
           ("C-r"   . nil)
           ("C-S-s" . isearch-repeat-backward)
           ("C-h"   . isearch-del-char)
           ("C-l"   . isearch-yank-char)
           ("C-n"   . isearch-ring-advance)
           ("C-p"   . isearch-ring-retreat)
           ([remap keyboard-escape-quit] . isearch-abort))

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


(use-package mozc-isearch
  :after mozc)


(use-package highlight-symbol
  :defer t
  :bind* (("<f3>" . highlight-symbol-mode))
  :config
  (defun highlight-symbol-count ()))
