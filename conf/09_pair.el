(show-paren-mode 1)
(setq blink-matching-paren t)
;; (setq show-paren-style 'expression)
;; (set-face-background 'show-paren-match-face "gray10")
;; (set-face-foreground 'show-paren-match-face "SkyBlue")

(electric-pair-mode 1)
(setq electric-pair-delete-adjacent-pairs nil)
(make-variable-buffer-local 'electric-pair-pairs)
