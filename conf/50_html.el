;; (with-eval-after-load 'mmask
;;   (mmask-regist-extension-with-icase 'nxml-mode "htm" "html" "shtm" "shtml" "xhtm" "xhtml" "xml" "rdf" "xul"))

;; (~add-setup-hook-after-load 'mmask 'nxml-mode
;;   (setq moccur-grep-default-mask (mmask-get-regexp-string 'nxml-mode)))

;; (load "rng-auto.el" 't)

;; (setq nxml-slash-auto-complete-flag t)      ; スラッシュの入力で終了タグを自動補完
;; (setq nxml-child-indent 2)                  ; タグのインデント幅
;; (setq nxml-attribute-indent 2)              ; 属性のインデント幅
;; (setq nxml-bind-meta-tab-to-complete-flag t)
;; (setq nxml-sexp-element-flag t)             ; C-M-kで下位を含む要素全体をkillする
;; (setq nxml-char-ref-display-glyph-flag nil) ; グリフは非表示

;; (~add-setup-hook 'nxml-mode
;;   ;; 更新タイムスタンプの自動挿入
;;   (setq time-stamp-line-limit 10000)
;;   (when (not (memq 'time-stamp write-file-hooks))
;;     (setq write-file-hooks (cons 'time-stamp write-file-hooks)))
;;   (setq time-stamp-format "%3a %3b %02d %02H:%02M:%02S %:y %Z")
;;   (setq time-stamp-start "Last modified:[ \t]")
;;   (setq time-stamp-end "$")

;;   (setq auto-fill-mode -1)

;;   (local-set-key (kbd "M-h") nil)
;;   (setq max-lisp-eval-depth 5000)
;;   (setq max-specpdl-size 6000))

;; (with-eval-after-load 'rng-loc
;;   (add-to-list 'rng-schema-locating-files (concat user-emacs-directory "schema/schemas.xml")))


;; (use-package genrnc
;;   :after (rng-loc))


(use-package web-mode
  :defer t
  :custom ((web-mode-code-indent-offset 2)
           (web-mode-markup-indent-offset 2)
           (web-mode-attr-indent-offset 2)
           (web-mode-css-indent-offset 2)
           (web-mode-sql-indent-offset 2)
           (web-mode-enable-auto-indentation nil)))


(use-package slim-mode
  :defer t)


(use-package haml-mode
  :defer t)


(use-package pug-mode
  :defer t)
