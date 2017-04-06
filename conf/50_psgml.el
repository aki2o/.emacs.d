;;***********************************************************************
;; 要素挿入：C-c C-e  カスタム(sgml-custom-markupに指定項目)：C-c C-u RET
;; 属性値：C-c +
;; 終了：C-c /
;; 削除：C-c -
;; 変更：C-c =
;;***********************************************************************

;;; PSGML の設定
(autoload 'sgml-mode "psgml" "Major mode to edit SGML files." t)
(autoload 'xml-mode "psgml" "Major mode to edit XML files." t)

;; カタログファイルの指定
(setq sgml-catalog-files '("~/DTD/xhtml11-20070216/DTD/xhtml11.cat"))

;; DOCTYPE 宣言の設定
(setq sgml-custom-dtd
      '(("XHTML 1.1"
         "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\"
                      \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">")
	("XHTML 1.0"
	 "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"
                      \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">")
        ))

;; sgml-mode hookで変数をsetq
(add-hook 'sgml-mode-hook
          '(lambda ()
             (setq tab-width                             2
                   sgml-indent-step                      2
                   sgml-indent-data                      t
                   indent-tabs-mode                      nil
                   sgml-xml-p                            t
                   sgml-always-quote-attributes          t
                   sgml-system-identifiers-are-preferred t
                   sgml-auto-activate-dtd                t
                   sgml-recompile-out-of-date-cdtd       t
                   sgml-auto-insert-required-elements    t
                   sgml-insert-missing-element-comment   t
                   sgml-balanced-tag-edit                t
                   sgml-default-doctype-name             "XHTML 1.1"
                   sgml-ecat-files                       nil
                   sgml-general-insert-case              'lower
                   sgml-entity-insert-case               'lower
                   sgml-normalize-trims                  t
                   sgml-insert-defaulted-attributes      nil
                   sgml-live-element-indicator           t
                   sgml-active-dtd-indicator             t
                   sgml-minimize-attributes              nil
                   sgml-omittag                          nil
                   sgml-omittag-transparent              nil
                   sgml-shorttag                         nil
                   sgml-tag-region-if-active             t
                   sgml-xml-validate-command             "xmllint --noout --valid %s %s"
                   )
             )
          t)

;;; これ以下はお好みで
;;; font-lock
;; (font-lock-mode 1)
;; (setq font-lock-support-mode   'jit-lock-mode
;;       jit-lock-stealth-verbose nil
;;       font-lock-verbose nil)
;;; PSGML デフォルトのfont-lockを使う場合
;;(setq sgml-set-face t
;;    sgml-markup-faces '((start-tag  . font-lock-builtin-face)
;;                          (end-tag    . font-lock-builtin-face)
;;                          (ms-start   . font-lock-variable-name-face)
;;                          (ms-end     . font-lock-variable-name-face)
;;                          (comment    . font-lock-comment-face)
;;                          (ignored    . font-lock-warning-face)
;;                          (pi         . font-lock-preprocessor-face)
;;                          (sgml       . font-lock-type-face)
;;                          (doctype    . font-lock-constant-face)
;;                          (entity     . font-lock-string-face)
;;                          (shortref   . font-lock-reference-face)))
;; My original font-lock-keywords
;; (add-hook 'sgml-mode-hook
;;           '(lambda ()
;;              (make-local-variable 'font-lock-defaults)
;;              (setq sgml-set-face nil
;;                    font-lock-defaults '(xml-font-lock-keywords-2 nil))
;;              (turn-on-font-lock)
;;              ))
;; (defvar xml-font-lock-keywords-1
;;   (list
;;    ;; タグ開始区切子 & タグ終了区切子
;;    '("<\\|>" 0 font-lock-keyword-face t)
;;    ;; スラッシュ
;;    '("\\(/\\)>" 1 font-lock-keyword-face t)
;;    '("<\\(/\\)" 1 font-lock-keyword-face t)
;;    ;; 要素名
;;    '("\\(</?\\)\\([a-zA-Z]+[a-zA-Z0-9-_:]*\\)" 2  font-lock-builtin-face t)
;;    ;; コメント
;;    '("\\(<!--\\([^-]\\|-[^-]\\|--[^>]\\)*-->\\)" 1 font-lock-comment-face t)
;;    ;; 命令処理
;;    '("\\(<\\?[a-zA-Z]*\\>[^<>]*\\(<[^>]*>[^<>]*\\)*\\?>\\)" 1 font-lock-type-face t)
;;    ;; DOCTYPE, ENTITY, ATTLIST, NOTATION等々 マーク宣言
;;    '("\\(<![a-zA-Z]+\\>[^<>]*\\(<[^>]*>[^<>]*\\)*>\\)" 1 font-lock-constant-face t)
;;    ;; °
;;    '("\\<\\([a-zA-Z]+[a-zA-Z-_:]*\\)=" 1 font-lock-variable-name-face t)
;;    ;; 属性値
;;    '("=?\\(\"[^\"]*\"\\|'[^\']*'\\)" 1 font-lock-string-face t)
;;    ;; 数値文字参照, 文字実体参照, パラメータ実体参照
;;    '("\\(&#[0-9]+;\\|&[a-zA-Z]+;\\|%[^'\";]+;\\)" 1 font-lock-reference-face t)
;;    ;; CDATA 等々 マーク区間 (マーク指定区域)
;;    '("\\(<!\\[[^\\[]+\\[[^]]+]]>\\)" 1 font-lock-warning-face t)
;;    ))
;; (defvar xml-font-lock-keywords-2
;;   (append
;;    xml-font-lock-keywords-1
;;    (list
;;     ;; SSI
;;     `(,(concat "\\(<!--#\\(fsize\\|flastmod\\|printenv\\|"
;;                "include\\|echo\\|config\\|exec\\|set\\|"
;;                "if\\|elif\\|else\\|endif\\)\\>[ \t\n]+"
;;                "\\([^-]\\|-[^-]\\|--[^>]\\)*-->\\)")
;;       1 'bold t)
;;     ;; php
;;     '("\\(<\\?\\(php\\|=\\)[^?>]+\\?>\\)" 1 font-lock-function-name-face t)
;;     ;; eRuby, JSP, ASP
;;     '("\\(<%\\(=\\)?\\>[^%>]+%>\\)" 1 font-lock-function-name-face t)
;;     )))
