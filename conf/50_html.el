(with-eval-after-load 'mmask
  (mmask-regist-extension-with-icase 'nxml-mode "htm" "html" "shtm" "shtml" "xhtm" "xhtml" "xml" "rdf" "xul"))

(load "rng-auto.el" 't)

(setq nxml-slash-auto-complete-flag t)      ; スラッシュの入力で終了タグを自動補完
(setq nxml-child-indent 2)                  ; タグのインデント幅
(setq nxml-attribute-indent 2)              ; 属性のインデント幅
(setq nxml-bind-meta-tab-to-complete-flag t)
(setq nxml-sexp-element-flag t)             ; C-M-kで下位を含む要素全体をkillする
(setq nxml-char-ref-display-glyph-flag nil) ; グリフは非表示

(add-hook 'nxml-mode-hook '~nxml-mode-setup t)
(defun ~nxml-mode-setup ()
  ;; 更新タイムスタンプの自動挿入
  (setq time-stamp-line-limit 10000)
  (when (not (memq 'time-stamp write-file-hooks))
    (setq write-file-hooks (cons 'time-stamp write-file-hooks)))
  (setq time-stamp-format "%3a %3b %02d %02H:%02M:%02S %:y %Z")
  (setq time-stamp-start "Last modified:[ \t]")
  (setq time-stamp-end "$")

  (setq auto-fill-mode -1)

  (local-set-key (kbd "M-h") nil)
  (setq max-lisp-eval-depth 5000)
  (setq max-specpdl-size 6000)

  (when (find-library-name "mmask")
    (setq moccur-grep-default-mask (mmask-get-regexp-string 'nxml-mode))))

(with-eval-after-load 'rng-loc
  (add-to-list 'rng-schema-locating-files (concat user-emacs-directory "schema/schemas.xml")))


(bundle genrnc :type github :pkgname "aki2o/emacs-genrnc")
(use-package genrnc
  :after (rng-loc))


(bundle slim-mode)
(use-package slim-mode
  :defer t)


(bundle haml-mode)
(use-package haml-mode
  :defer t)


(bundle pug-mode)
(use-package pug-mode
  :defer t)

