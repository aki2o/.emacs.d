(use-package grep
  :defer t
  :commands (~grep-by-git)
  :init
  (setq grep-host-defaults-alist nil) ; これはおまじないだと思ってください
  (setq grep-template "grep <C> -n <R> <F> <N>")
  (setq grep-find-template "find . <X> -type f <F> -print0 | xargs -0 -e grep <C> -n <R> <N>")
  ;; ;; Shift_JISを使うように設定
  ;; (setq grep-template "lgrep -Ks -Os <C> -n <R> <F> <N>")
  ;; (setq grep-find-template "find . <X> -type f <F> -print0 | xargs -0 -e lgrep -Ks -Os <C> -n <R> <N>")

  :config
  ;; el-getのトラブルで調査した際、なくてもOKっぽかったのでコメントアウト
  ;; ;; shell-quote-argumentの問題回避
  ;; (defadvice shell-quote-argument (around shell-quote-argument-for-win activate)
  ;;   "workaround for windows."
  ;;   (if (eq system-type 'windows-nt)
  ;;       (let ((argument (ad-get-arg 0)))
  ;;         (setq argument (replace-regexp-in-string "\\\\" "\\\\" argument nil t))
  ;;         (setq argument (replace-regexp-in-string "'" "'\\''" argument nil t))
  ;;         (setq ad-return-value (concat "'" argument "'")))
  ;;     ad-do-it))
  )

(defun ~grep-by-git (regexp &optional dir)
  (interactive
   (list (read-string "Regexp: ")))
  (let ((cmd (format "PAGER='' git grep -I -n -i -e %s"
                     (shell-quote-argument regexp)))
        (default-directory (if dir (expand-file-name dir) default-directory))
        (null-device nil))
    (grep cmd)))


(bundle igrep)
(use-package igrep
  :after (grep)
  :config
  ;; lgrepを使い、UTF-8で出力
  (igrep-define lgrep (igrep-use-zgrep nil) (igrep-regex-option "-n -Ou8"))
  (igrep-find-define lgrep (igrep-use-zgrep nil) (igrep-regex-option "-n -Ou8"))
  ;; (igrep-define grep (igrep-use-zgrep nil) (igrep-regex-option "-n"))
  ;; (igrep-find-define grep (igrep-use-zgrep nil) (igrep-regex-option "-n"))
  )


(bundle grep-a-lot)
(use-package grep-a-lot
  :after (grep)
  :config
  ;; (grep-a-lot-setup-keys)
  (grep-a-lot-advise igrep))


(bundle wgrep :type github :pkgname "mhayashi1120/Emacs-wgrep")
(use-package wgrep
  :custom ((wgrep-enable-key "e")
           (wgrep-auto-save-buffer t))
  :after (grep))


(bundle ag)
(use-package ag
  :defer t
  :commands (~ag pophint-thing:just-~ag)
  :config
  (custom-set-variables
   '(ag-highlight-search t)  ; 検索結果の中の検索語をハイライトする
   '(ag-reuse-window 'nil)   ; 現在のウィンドウを検索結果表示に使う
   '(ag-reuse-buffers 'nil)) ; 現在のバッファを検索結果表示に使う

  (with-eval-after-load 'pophint
    (pophint-thing:advice-thing-at-point-function ag/dwim-at-point)
    (pophint-thing:defcommand-noadvice ~ag))
  )

(defun ~ag ()
  (interactive)
  (call-interactively (if current-prefix-arg 'ag-regexp 'ag)))


(bundle wgrep-ag :type github :pkgname "mhayashi1120/Emacs-wgrep")
(use-package wgrep-ag
  :after (ag)
  :hook (ag-mode . wgrep-setup))


(bundle rg)
(use-package rg
  :defer t)

