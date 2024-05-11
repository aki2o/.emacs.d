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

(advice-add 'electric-pair-post-self-insert-function :around #'my:electric-pair-dwim-setup)
(advice-add 'electric-pair--insert :around #'my:electric-pair-dwim)

(defvar my:electric-pair-dwim-done-p nil)
(defvar my:electric-pair-dwim-last-arg nil)
(defvar my:electric-pair-dwim-working-p nil)

(defun my:electric-pair-dwim-setup (orig &rest args)
  (apply orig args)
  ;; electric-pair--insert からも呼ばれるので、その場合はスキップ
  (when (not my:electric-pair-dwim-working-p)
    (when (not my:electric-pair-dwim-done-p)
      (setq my:electric-pair-dwim-last-arg nil))
    (setq my:electric-pair-dwim-done-p nil)))

(defun my:electric-pair-dwim (orig &rest args)
  (let ((arg (nth 0 args)))
    (cond ((and (eq last-command 'self-insert-command)
                (eq arg my:electric-pair-dwim-last-arg))
           ;; 続けて入力した場合は、前回入力を取り消す
           (setq my:electric-pair-dwim-last-arg nil)
           (backward-delete-char 1)
           (and (re-search-forward (format "%c" arg) nil t)
                (backward-delete-char 1)))
          (t
           (let* ((char (char-after))
                  (pair (assoc-default char electric-pair-pairs))
                  (my:electric-pair-dwim-working-p t))
             (cond (pair
                    ;; ペア開始文字上だったら、そのペア終了の後に挿入する
                    (forward-sexp))
                   ((and char
                         (string-match (rx word) (format "%c" char)))
                    ;; 単語文字上だったら、その後に挿入する
                    (or (when (re-search-forward (rx (or space "\n")) nil t)
                          (backward-char 1))
                        (goto-char (point-max)))))
             (setq my:electric-pair-dwim-done-p t)
             (setq my:electric-pair-dwim-last-arg arg)
             (apply orig args))))))
