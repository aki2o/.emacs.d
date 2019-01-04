(bundle emacswiki:anything)
(bundle emacswiki:anything-menu)
(bundle emacswiki:anything-obsolete)
(bundle emacswiki:anything-show-completion)

;; (require 'anything-startup)
;; はせずに、anything-startupの中身を個別に記述

(bundle emacswiki:anything-config)
(require 'anything-config)

(bundle emacswiki:anything-match-plugin)
(require 'anything-match-plugin)

(bundle emacswiki:anything-migemo)
(use-package anything-migemo
  :if (equal current-language-environment "Japanese"))

(bundle emacswiki:anything-complete)
(use-package anything-complete
  :config
  (setq anything-read-string-mode-flags '(string buffer variable))
  (anything-lisp-complete-symbol-set-timer 150)
  (define-key emacs-lisp-mode-map "\C-\M-i" 'anything-lisp-complete-symbol-partial-match)
  (define-key lisp-interaction-mode-map "\C-\M-i" 'anything-lisp-complete-symbol-partial-match)
  (anything-read-string-mode 1))

(bundle emacswiki:anything-show-completion)
(require 'anything-show-completion)

(bundle emacswiki:auto-install)
(bundle emacswiki:anything-auto-install)
(use-package anything-auto-install)

(bundle descbinds-anything)
(use-package descbinds-anything
  :config
  (descbinds-anything-install))

(bundle emacswiki:anything-grep)
(use-package anything-grep)


;; face
(custom-set-faces
 '(anything-ff-file ((t (:inherit default))))
 '(anything-ff-directory ((t (:inherit dired-directory))))
 ;; '(anything-ff-executable ((t (:inherit default))))
 ;; '(anything-ff-symlink ((t (:inherit default))))
 ;; '(anything-ff-invalid-symlink ((t (:inherit default))))
 )


;; anything動作中のキーバインド
(defvar ~anything-modify-keymap-required nil)
(defvar ~anything-modify-keymap-finished nil)

(defadvice anything-approximate-candidate-number (before mod-keymap activate)
  (when (and ~anything-modify-keymap-required
             (not ~anything-modify-keymap-finished))
    (define-key anything-map (kbd "C-j")   'anything-next-line)
    (define-key anything-map (kbd "C-k")   'anything-previous-line)
    (define-key anything-map (kbd "C-h")   'anything-previous-source)
    (define-key anything-map (kbd "C-l")   'anything-next-source)
    (define-key anything-map (kbd "C-S-j") 'anything-next-page)
    (define-key anything-map (kbd "C-S-k") 'anything-previous-page)
    (define-key anything-map (kbd "C-{")   'anything-beginning-of-buffer)
    (define-key anything-map (kbd "C-}")   'anything-end-of-buffer)
    ;; (define-key anything-map (kbd "C-f")   'anything-delete-minibuffer-contents)
    (setq ~anything-modify-keymap-finished t)))

(defadvice anything-read-pattern-maybe (around mod-keymap activate)
  (let ((~anything-modify-keymap-required t)
        (~anything-modify-keymap-finished nil))
    ad-do-it))

;; firefoxのブックマークフォルダを変更
(defun anything-get-firefox-user-init-dir ()
  (concat user-emacs-directory "firefox/"))

;; anything-buffers-listをカスタマイズ
(setq anything-allow-skipping-current-buffer t)
(setq anything-c-boring-buffer-regexp (rx-to-string `(and bos (or " *Echo Area" " *Minibuf"))))

;; anything-find-fileで入力にマッチする候補が一つになった時点で自動補完はしない
(setq anything-ff-auto-update-initial-value nil)

;; ;; anything-for-filesにバッファ一覧は不要
;; (setq anything-for-files-prefered-list
;;       (delq 'anything-c-source-buffers-list anything-for-files-prefered-list))

;; 各コマンドのキーマップで代替コマンド定義用
(defmacro ~anything-defun-quit-and-execute-action (func)
  `(defun ,(intern (concat "~anything-run-" (symbol-name func))) ()
     (interactive)
     (anything-c-quit-and-execute-action ',func)))

(define-key anything-c-buffer-map (kbd "C-o") 'anything-buffer-switch-other-window)
(define-key anything-c-buffer-map (kbd "C-S-c s") 'anything-buffer-run-grep)
(define-key anything-c-buffer-map (kbd "C-S-c S") 'anything-buffer-run-zgrep)
(define-key anything-c-buffer-map (kbd "C-S-c o") 'anything-buffer-switch-other-window)
(define-key anything-c-buffer-map (kbd "C-S-c e") 'anything-buffer-run-ediff)
(define-key anything-c-buffer-map (kbd "C-S-c E") 'anything-buffer-run-ediff-merge)
(define-key anything-c-buffer-map (kbd "C-S-c u") 'anything-buffer-revert-persistent)
(define-key anything-c-buffer-map (kbd "C-S-c d") 'anything-buffer-run-kill-buffers)
(define-key anything-c-buffer-map (kbd "C-S-c r") 'anything-buffer-run-query-replace)
(define-key anything-c-buffer-map (kbd "C-S-c m") 'anything-toggle-all-marks)
(define-key anything-c-buffer-map (kbd "C-S-c a") 'anything-mark-all)

(~anything-defun-quit-and-execute-action anything-find-files-grep)
(~anything-defun-quit-and-execute-action find-file-other-window)
(~anything-defun-quit-and-execute-action anything-find-files-ediff-files)
(~anything-defun-quit-and-execute-action anything-find-files-ediff-merge-files)

(define-key anything-generic-files-map (kbd "C-o") '~anything-run-find-file-other-window)
(define-key anything-generic-files-map (kbd "C-S-c s") '~anything-run-anything-find-files-grep)
(define-key anything-generic-files-map (kbd "C-S-c o") '~anything-run-find-file-other-window)
(define-key anything-generic-files-map (kbd "C-S-c e") '~anything-run-anything-find-files-ediff-files)
(define-key anything-generic-files-map (kbd "C-S-c E") '~anything-run-anything-find-files-ediff-merge-files)
(define-key anything-generic-files-map (kbd "C-S-c d") '~anything-run-anything-delete-marked-files)

(define-key anything-find-files-map (kbd "C-o") '~anything-run-find-file-other-window)
(define-key anything-find-files-map (kbd "C-S-c s") '~anything-run-anything-find-files-grep)
(define-key anything-find-files-map (kbd "C-S-c S") 'anything-ff-zgrep)
(define-key anything-find-files-map (kbd "C-S-c o") '~anything-run-find-file-other-window)
(define-key anything-find-files-map (kbd "C-S-c e") '~anything-run-anything-find-files-ediff-files)
(define-key anything-find-files-map (kbd "C-S-c E") '~anything-run-anything-find-files-ediff-merge-files)
(define-key anything-find-files-map (kbd "C-S-c d") '~anything-run-anything-delete-marked-files)
(define-key anything-find-files-map (kbd "C-S-c c") 'anything-find-files-copy)
(define-key anything-find-files-map (kbd "C-S-c C") 'anything-ff-copy-async)
(define-key anything-find-files-map (kbd "C-S-c r") 'anything-find-files-rename)
(define-key anything-find-files-map (kbd "C-S-c R") 'anything-ff-serial-rename)
(define-key anything-find-files-map (kbd "C-S-c p") 'anything-ff-print)
(define-key anything-find-files-map (kbd "C-S-h") 'anything-find-files-down-one-level)
(define-key anything-find-files-map (kbd "C-S-l") 'anything-execute-persistent-action)

;; pop-tag-markで戻れるようにする
(defun ~anything-c-etags-select-with-saved-point ()
  "call anything-c-etags-select with point of marker saved."
  (interactive)
  (ring-insert find-tag-marker-ring (point-marker))
  (when (and tags-file-name
             (file-exists-p tags-file-name))
    (setq anything-c-etags-tag-file-name tags-file-name))
  (anything-c-etags-select '(4)))

;; シェル履歴
(anything-complete-shell-history-setup-key (kbd "C-M-p"))

;; Migemoを使うコマンド
(anything-migemize-command anything-find-files)
(anything-migemize-command anything-for-files)
(anything-migemize-command anything-buffers-list)
(anything-migemize-command anything-show-kill-ring)
(anything-migemize-command anything-w3m-bookmarks)
(anything-migemize-command anything-bbdb)

;; やっぱし通常でよさげ
;; (defadvice anything-c-highlight-buffers (after ~mycustomize activate)
;;   (setq ad-return-value
;;         (sort (loop with currmode = (with-current-buffer anything-current-buffer major-mode)
;;                     for buffnm in ad-return-value
;;                     for buff = (get-buffer buffnm)
;;                     for bfmode = (with-current-buffer buff major-mode)
;;                     collect (cond
;;                              ;; 同じモードのバッファ
;;                              ((eq bfmode currmode)
;;                               (propertize buffnm 'face 'font-lock-function-name-face))
;;                              ;; Diredバッファ
;;                              ((rassoc buff dired-buffers)
;;                               (propertize buffnm 'face 'font-lock-keyword-face))
;;                              ;; 隠しバッファ
;;                              ((string-match "\\` " buffnm)
;;                               (propertize buffnm 'face 'font-lock-comment-face))
;;                              ;; 拡張が使うバッファ
;;                              ((string-match "\\`\\*" buffnm)
;;                               (propertize buffnm 'face 'font-lock-string-face))
;;                              ;; デフォルトに戻す
;;                              ((eq (get-text-property 0 'face buffnm) 'font-lock-type-face)
;;                               (propertize buffnm 'face 'default))
;;                              (t
;;                               buffnm)))
;;               '~anything-c-buffer-sort)))

;; (defun ~anything-c-buffer-sort (b1 b2)
;;   (multiple-value-bind (t1 t2)
;;       (loop with currmode = (with-current-buffer anything-current-buffer major-mode)
;;             for buffnm in (list b1 b2)
;;             for bfmode = (with-current-buffer buffnm major-mode)
;;             collect (cond ((eq bfmode currmode)           'samemode)
;;                           ((string-match "\\`\\*" buffnm) 'app)
;;                           ((string-match "\\` " buffnm)   'hide)
;;                           (t                              nil)))
;;     ;; samemode > nil > app > hide の順で並び替え
;;     (case t1
;;       (samemode (case t2
;;                   (hide     t)
;;                   (app      t)
;;                   (samemode nil)
;;                   (t        t)))
;;       (app      (case t2
;;                   (hide     t)
;;                   (app      nil)
;;                   (samemode nil)
;;                   (t        nil)))
;;       (hide     (case t2
;;                   (hide     nil)
;;                   (app      nil)
;;                   (samemode nil)
;;                   (t        nil)))
;;       (t        (case t2
;;                   (hide     t)
;;                   (app      t)
;;                   (samemode nil)
;;                   (t        nil))))))

;; anything-find-fileでデフォルトでカレントディレクトリを開くようにする
(defadvice anything-find-files-1 (before ~disable-preselect activate)
  (ad-set-arg 1 nil))


(bundle k1LoW/anything-replace-string)
(use-package anything-replace-string
  :commands (anything-replace-string))

