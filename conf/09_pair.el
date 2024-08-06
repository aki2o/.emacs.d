(show-paren-mode 1)
(setq blink-matching-paren t)
;; (setq show-paren-style 'expression)
;; (set-face-background 'show-paren-match-face "gray10")
;; (set-face-foreground 'show-paren-match-face "SkyBlue")

(electric-pair-mode 1)
(setq electric-pair-delete-adjacent-pairs nil)

(add-to-list 'electric-pair-pairs '(?' . ?'))
(add-to-list 'electric-pair-pairs '(?{ . ?}))
(add-to-list 'electric-pair-pairs '(?[ . ?]))
(add-to-list 'electric-pair-pairs '(?( . ?)))
(make-variable-buffer-local 'electric-pair-pairs)

(advice-add 'electric-pair--insert :around #'my:electric-pair-dwim)

(defun my:electric-pair-dwim (orig &rest args)
  (let* ((char (char-after))
         (pair (assoc-default char electric-pair-pairs)))
    (when (not (region-active-p))
      (cond (pair
             ;; ペア開始文字上だったら、そのペア終了の後に挿入する
             (forward-sexp))
            ((and char
                  (string-match (rx word) (format "%c" char)))
             ;; 単語文字上だったら、その後に挿入する
             (or (re-search-forward (rx word-end) (pos-eol) t)
                 (re-search-forward (rx line-end) nil t)
                 (goto-char (point-max))))))
    (apply orig args)))

(add-hook 'minibuffer-setup-hook '~electric-pair-setup-for-minibuffer)

(defun ~electric-pair-setup-for-minibuffer ()
  (setq electric-pair-pairs (-remove (lambda (x) (eq (car x) ?')) electric-pair-pairs)))
