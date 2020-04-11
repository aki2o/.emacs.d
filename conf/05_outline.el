;; IMenu
(setq imenu-max-item-length 150)


;; Outline
(defun ~outline-current-level-headings ()
  (interactive)
  (save-excursion
    (outline-show-all)                             ; 全て表示して、
    (outline-back-to-heading t)                    ; 今いる見出しに移動し、
    (call-interactively 'outline-hide-sublevels))) ; 同じレベルの階層の見出しだけ表示

(defun ~outline-all-headings ()
  (interactive)
  (save-excursion
    (outline-show-all)          ; 全て表示して、
    (outline-back-to-heading t) ; 今いる見出しに移動し、
    (outline-hide-body)))       ; 見出し以外を隠す

(defun ~outline-narrow-current-heading ()
  (interactive)
  (save-excursion
    (outline-back-to-heading t) ; 今いる見出しに移動し、
    (outline-hide-other)        ; 今いる見出し以外を全て隠し、
    (outline-show-subtree)))    ; 配下を全て表示

(defun ~outline-current-child-headings ()
  (interactive)
  (save-excursion
    (outline-back-to-heading t) ; 今いる見出しに移動し、
    (outline-hide-subtree)      ; 配下を全て隠し、
    (outline-show-children)))   ; 直下の見出しだけ表示

(defun ~outline-current-all-headings ()
  (interactive)
  (save-excursion
    (outline-back-to-heading t) ; 今いる見出しに移動し、
    (outline-hide-subtree)      ; 配下を全て隠し、
    (outline-show-branches)))   ; 配下の見出しだけ表示

(defun ~outline-current-all-headings-with-body ()
  (interactive)
  (save-excursion
    (outline-back-to-heading t) ; 今いる見出しに移動し、
    (outline-hide-subtree)      ; 配下を全て隠し、
    (outline-show-entry)        ; 直下の見出し以外と、
    (outline-show-branches)))   ; 配下の見出しだけ表示

(defun ~outline-current-all-body ()
  (interactive)
  (save-excursion
    (outline-back-to-heading t) ; 今いる見出しに移動し、
    (outline-show-subtree)))    ; 配下を全て表示

(defvar ~outline-mode-alist
  `((ruby-mode . ((level . (lambda () (- (match-end 1) (match-beginning 1))))
                 (regexp . ,(rx (group (0+ (in " \t"))) ; インデント数を階層の深さにするため、グルーピング
                                (or
                                 ;; クラス定義とか
                                 (and (or "class" "module" "def") " ")
                                 ;; do ... end
                                 (and (any "a-zA-Z@") (1+ (any "0-9a-zA-Z_:")) eow ; describe や resources などのメソッド名があり、
                                      (0+ not-newline) " do" eow                   ; 引数が続いた後、 `do' があり、
                                      (0+ (not (any "'\""))) eol)                  ; 後続に文字列クォートが無いなら、 do...end とみなす（ブロック引数のデフォルトが文字列リテラルだったりすると弾けないけど、まあ無いやろ
                                 ;; { ... }
                                 (and (1+ not-newline) " {"     ; `{' があって、
                                      (0+ (not (any "}"))) eol) ; `}' が無いなら、 {...} とみなす
                                 ;; [ ... ]
                                 (and (1+ not-newline) " ["     ; `[' があって、
                                      (0+ (not (any "]"))) eol) ; `]' が無いなら、 [...] とみなす
                                 ;; private も見出しとして扱ってみる
                                 (and "private" eol)
                                 )))))))

(dolist (entry ~outline-mode-alist)
  (let ((mode (car entry)))
    (add-hook
     (intern (format "%s-hook" mode))
     `(lambda ()
        (let ((conf (assoc-default ',mode ~outline-mode-alist)))
          (setq outline-level (or (assoc-default 'level conf) 'outline-level))
          (setq outline-heading-alist (or (assoc-default 'heading-alist conf) ()))
          (setq outline-regexp (or (assoc-default 'regexp conf) outline-regexp)))
        (outline-minor-mode 1))
     t)))
