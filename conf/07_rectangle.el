;; ;; cuaの矩形選択機能だけ使う
;; (cua-mode t)
;; (setq cua-enable-cua-keys nil) ; C-cやC-vの乗っ取りを阻止
;; (setq cua-highlight-region-shift-only nil)

(unbind-key "C-x C-r")
(bind-keys* ("C-x C-r a" . cua-align-rectangle)
            ("C-x C-r b" . cua-blank-rectangle)
            ("C-x C-r s" . cua-string-rectangle)
            ("C-x C-r f" . cua-fill-char-rectangle)
            ("C-x C-r i" . cua-incr-rectangle)
            ("C-x C-r r" . cua-replace-in-rectangle)
            ("C-x C-r n" . cua-sequence-rectangle))

