(bundle igrep)
(bundle grep-a-lot)
(bundle emacswiki:grep-edit)
(use-package grep
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
  
  (use-package igrep
    :config
    ;; lgrepを使い、UTF-8で出力
    (igrep-define lgrep (igrep-use-zgrep nil) (igrep-regex-option "-n -Ou8"))
    (igrep-find-define lgrep (igrep-use-zgrep nil) (igrep-regex-option "-n -Ou8"))
    ;; (igrep-define grep (igrep-use-zgrep nil) (igrep-regex-option "-n"))
    ;; (igrep-find-define grep (igrep-use-zgrep nil) (igrep-regex-option "-n"))
    )

  (use-package grep-a-lot
    :config
    ;; (grep-a-lot-setup-keys)
    (grep-a-lot-advise igrep))

  (use-package grep-edit)

  (defun ~grep-by-git (regexp dir)
    (interactive
     (list (read-string "Regexp: ")
           (read-directory-name "Dir: ")))
    (let ((cmd (format "PAGER='' git grep -I -n -i -e %s"
                       (shell-quote-argument regexp)))
          (default-directory (expand-file-name dir))
          (null-device nil))
      (grep cmd)))

  )


(bundle ag)
(bundle wgrep)
(bundle mhayashi1120/Emacs-wgrep :name wgrep-helm)
(bundle wgrep-ag)
(use-package ag
  :commands (~ag pophint-thing:just-~ag)
  
  :config
  (custom-set-variables
   '(ag-highlight-search t)  ; 検索結果の中の検索語をハイライトする
   '(ag-reuse-window 'nil)   ; 現在のウィンドウを検索結果表示に使う
   '(ag-reuse-buffers 'nil)) ; 現在のバッファを検索結果表示に使う

  (defun ~ag ()
    (interactive)
    (call-interactively (if current-prefix-arg 'ag-regexp 'ag)))
  
  (use-package wgrep-ag
    :config
    (add-hook 'ag-mode-hook 'wgrep-ag-setup t)
    (bind-keys :map ag-mode-map
               ("r" . wgrep-change-to-wgrep-mode)))
  )

(bundle rg)
(use-package rg
  :defer t)


(bundle helm-ag)
(use-package helm-ag
  :commands (~helm-ag pophint-thing:just-~helm-ag)
  
  :init
  (setq helm-ag-insert-at-point 'pophint)
  
  :config
  (defun ~helm-ag (directory)
    (interactive (list (read-directory-name "Directory: ")))
    (helm-ag directory))
  )


(bundle helm-rg)
(use-package helm-rg
  :defer t)


(bundle helm-git-grep)
(use-package helm-git-grep
  :defer t)


(use-package counsel
  :commands (~counsel-ag ~counsel-git-grep)
  
  :init
  (defun ~counsel-initial-input ()
    (thing-at-point 'word))
  
  (defun ~counsel-ag ()
    (interactive)
    (counsel-ag (~counsel-initial-input) (read-directory-name "Dir: ")))

  (defun ~counsel-git-grep ()
    (interactive)
    (counsel-git-grep nil (~counsel-initial-input)))
  )


(bundle swiper)
(use-package swiper
  :commands (~swiper)
  
  :init
  (defun ~swiper ()
    (interactive)
    (swiper (~counsel-initial-input))))

