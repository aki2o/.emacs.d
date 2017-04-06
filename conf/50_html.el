;; (bundle web-mode)
(bundle auto-complete-nxml)
(use-package nxml-mode
  :defer t

  :init
  
  (mmask-regist-extension-with-icase 'nxml-mode "htm" "html" "shtm" "shtml" "xhtm" "xhtml" "xml" "rdf" "xul")
  (load "rng-auto.el" 't)

  :config
  
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
    (setq nxml-slash-auto-complete-flag t)      ; スラッシュの入力で終了タグを自動補完
    (setq nxml-child-indent 2)                  ; タグのインデント幅
    (setq nxml-attribute-indent 2)              ; 属性のインデント幅
    (setq nxml-bind-meta-tab-to-complete-flag t) 
    (setq nxml-sexp-element-flag t)             ; C-M-kで下位を含む要素全体をkillする
    (setq nxml-char-ref-display-glyph-flag nil) ; グリフは非表示
    
    (local-set-key (kbd "M-h") nil)
    (setq max-lisp-eval-depth 5000)
    (setq max-specpdl-size 6000)

    ;; color-moccur
    (setq moccur-grep-default-mask (mmask-get-regexp-string 'nxml-mode)))

  ;; custom-set-faces
  ;;  '(nxml-comment-content-face
  ;;    ((t (:foreground "red"))))                            ; コメント
  ;;  '(nxml-comment-delimiter-face
  ;;    ((t (:foreground "red"))))                            ; ＜!-- --＞
  ;;  '(nxml-delimited-data-face
  ;;    ((t (:foreground "DarkViolet"))))                     ; 属性値やDTD引数値など
  ;;  '(nxml-delimiter-face
  ;;    ((t (:foreground "blue"))))                           ; ＜＞ ＜? ?＞ ""
  ;;  '(nxml-element-local-name-face
  ;;    ((t (:inherit nxml-name-face :foreground "blue"))))   ; 要素名
  ;;  '(nxml-name-face
  ;;    ((t (:foreground "dark green"))))                     ; 属性名など
  ;;  '(nxml-element-colon-face
  ;;    ((t (:foreground "LightSteelBlue"))))                 ; :(xsl:paramなど)
  ;;  '(nxml-ref-face
  ;;    ((t (:foreground "DarkGoldenrod"))))                  ; ＆lt;など
  ;;  '(nxml-tag-slash-face
  ;;    ((t (:inherit nxml-name-face :foreground "blue")))))  ; /(終了タグ)

  ;; flymake
  (defun flymake-nxml-init ()
    (let ((temp-file (flymake-init-create-temp-buffer-copy 'flymake-create-temp-inplace)))
      (list "xmlstarlet" (list "val" "-e" "-w" temp-file))))
  
  (push (list (mmask-get-regexp-string 'nxml-mode) 'flymake-nxml-init) flymake-allowed-file-name-masks)

  (use-package auto-complete-nxml
    :config
    (setq auto-complete-nxml-popup-help-key "C-'")
    (setq auto-complete-nxml-toggle-automatic-key "C-c C-t"))

  )


(bundle genrnc)
(use-package rng-loc
  :defer t
  :config
  (add-to-list 'rng-schema-locating-files (concat user-emacs-directory "schema/schemas.xml"))
  (require 'genrnc))

