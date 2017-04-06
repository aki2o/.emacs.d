(bundle igrep)
(bundle grep-a-lot)
(bundle emacswiki:grep-edit)
(use-package grep
  :bind* (("M-s l" . lgrep)
          ("M-s r" . rgrep)
          ("M-s G" . ~grep-by-git))

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
(bundle mhayashi1120/Emacs-wgrep :name wgrep-helm :checkout "f8099401c2c7007cdb9c3e4fc1eafb5329f72031")
(bundle wgrep-ag)
(use-package ag
  :bind* (("M-s a" . ~ag)
          ("M-s A" . pophint-config:thing-do-~ag-with-toggle-effect))

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

  (use-package pophint-config
    :config
    (pophint-config:set-thing-at-point-function ag/dwim-at-point)
    (pophint-config:thing-def-command-with-toggle-effect ~ag))
  )


(bundle helm-ag)
(use-package helm-ag
  :bind* (("M-s h a" . ~helm-ag)
          ("M-s h A" . pophint-config:thing-do-~helm-ag-with-toggle-effect))

  :init

  (setq helm-ag-insert-at-point 'pophint)
  
  :config

  (defun ~helm-ag (directory)
    (interactive (list (read-directory-name "Directory: ")))
    (helm-ag directory))

  (use-package pophint-config
    :config
    (pophint-config:set-thing-at-point-function helm-ag--insert-thing-at-point)
    (pophint-config:thing-def-command-with-toggle-effect ~helm-ag))
  )


(bundle yasuyk/helm-git-grep :checkout "9e602f79ea58fe12c6a48ce3c2f749b817ef8c86")
(use-package helm-git-grep
  :bind* (("M-s h g" . helm-git-grep))
  )


(use-package counsel
  :bind* (("M-s c a" . ~counsel-ag)
          ("M-s c g" . ~counsel-git-grep))

  :init

  (defun ~counsel-initial-input ()
    (thing-at-point 'word))
  
  (defun ~counsel-ag ()
    (interactive)
    (counsel-ag (~counsel-initial-input) (read-directory-name "Dir: ")))

  (defun ~counsel-git-grep ()
    (interactive)
    (counsel-git-grep nil (~counsel-initial-input)))

  :config
  
  (use-package pophint-config
    :config
    (pophint-config:set-thing-at-point-function ~counsel-initial-input))
  )


(bundle swiper)
(use-package swiper
  :bind* (("M-s I" . ~swiper))

  :init

  (defun ~swiper ()
    (interactive)
    (swiper (~counsel-initial-input))))

