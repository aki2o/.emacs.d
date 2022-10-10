(require 'dired)

(setq wdired-allow-to-change-permissions t) ; ファイルのパーミッションを編集可能にする
(setq dired-dwim-target t)                  ; 2ウィンドウ時に、デフォルトの移動/コピー先をもう一方のウィンドウに
(setq dired-recursive-copies 'always)       ; ディレクトリを再帰的にコピー
(setq dired-isearch-filenames t)            ; C-sでファイル名だけにマッチ

(bind-keys :map dired-mode-map
           ("r" . wdired-change-to-wdired-mode)
           ("z" . ~dired-zip-files)
           ("W" . ~uenox-dired-winstart)
           ("E" . ~dired-exec-explorer) ; Win32: dierd からエクスプローラを開く
           
           ("C-n" . nil)
           ("C-j" . dired-next-line)
           ("C-p" . nil)
           ("C-k" . dired-previous-line)
           ("M-s" . nil))
  
(defun ~dired-zip-files (zip-file)
  (interactive "sEnter name of zip file: ")
  (let ((zip-file (if (string-match "\\.zip$" zip-file)
                      zip-file
                    (concat zip-file ".zip"))))
    (shell-command (format "zip %s %s"
                           zip-file
                           (mapconcat '(lambda (f)
                                         (format "'%s'" (file-name-nondirectory f)))
                                      (dired-get-marked-files)
                                      " ")))
    (revert-buffer)))

;; Dired で Windows に関連付けられたファイルを起動する。
(defun ~uenox-dired-winstart ()
  (interactive)
  (if (eq major-mode 'dired-mode)
      (let ((fname (dired-get-filename)))
        (w32-shell-execute "open" fname)
        (message "win-started %s" fname))))

;; ;; M-x explorer で現在のカレントディレクトリをもとにエクスプローラを立ち上げる設定。
;; (define-process-argument-editing "/explorer\\.exe$"
;;   (lambda (x)
;;     (general-process-argument-editing-function x nil nil nil)))

(defun ~explorer (&optional dir)
  (interactive)
  (setq dir (expand-file-name (or dir default-directory)))
  (if (or (not (file-exists-p dir))
          (and (file-exists-p dir)
               (not (file-directory-p dir))))
      (message "%s can't open." dir)
    (setq dir (unix-to-dos-filename dir))
    (let ((w32-start-process-show-window t))
      (apply (function start-process)
             "explorer" nil "explorer.exe" (list (concat "/e,/root," dir))))))

(defun ~dired-exec-explorer ()
  (interactive)
  (~explorer (dired-current-directory)))


(require 'dired-x)
(setq dired-guess-shell-gnutar "tar") ; Cygwin 利用時
(setq dired-guess-shell-alist-user
      '(("\\.tar\\.gz\\'"  "tar ztvf")
        ("\\.taz\\'" "tar ztvf")
        ("\\.tar\\.bz2\\'" "tar Itvf")
        ("\\.zip\\'" "unzip -l")
        ("\\.\\(g\\|\\) z\\'" "zcat")
        ("\\.\\(jpg\\|JPG\\|gif\\|GIF\\)\\'" (if (~is-windows) "fiber" "xv"))
        ("\\.ps\\'" (if (~is-windows) "fiber" "ghostview"))))


(require 'dired-aux)
;; .zipで終わるファイルをZキーで展開できるように
(add-to-list 'dired-compress-file-suffixes '("\\.zip\\'" ".zip" "unzip"))

;; idoやivyが起動してしまって、既存名にマッチすると新規作成できないので、
;; `read-file-name' の代わりに `read-string' を使う
(defun dired-create-directory (directory)
  (interactive
   (list (read-string "Create directory: " (dired-current-directory))))
  (let* ((expanded (directory-file-name (expand-file-name directory)))
         (try expanded) new)
    (if (file-exists-p expanded)
        (error "Cannot create directory %s: file exists" expanded))
    ;; Find the topmost nonexistent parent dir (variable `new')
    (while (and try (not (file-exists-p try)) (not (equal new try)))
      (setq new try
            try (directory-file-name (file-name-directory try))))
    (make-directory expanded t)
    (when new
      (dired-add-file new)
      (dired-move-to-filename))))
