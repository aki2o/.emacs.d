(use-package helm)
(require 'helm-config)
(require 'helm-files)
(require 'helm-for-files)

(global-unset-key (kbd "C-x h"))

(custom-set-variables
 '(helm-command-prefix-key "C-x h")
 '(helm-split-window-in-side-p t)
 '(helm-ff-candidate-number-limit 500)
 '(helm-ff-newfile-prompt-p nil)
 '(helm-file-name-case-fold-search t))

(bind-keys :map helm-command-map
           ("?" . helm-help)
           ("f" . helm-find-files)
           ("F" . helm-for-files)
           ("a" . helm-apropos)
           ("b" . helm-buffers-list)
           ("k" . helm-show-kill-ring)
           ("i" . helm-imenu)
           ("m" . helm-all-mark-rings)
           ("h" . helm-complex-command-history)
           ("r" . helm-resume)
           ("B" . helm-bookmark)
           ("l" . helm-locate-library))

(use-package helm-descbinds) ; replace from descbinds-anything
(helm-descbinds-install)  ; (descbinds-anything-install)
;; ;; descbinds-anythingも他のanythingコマンドと同じように
(setq helm-descbinds-window-style 'split-window) ; (setq descbinds-anything-window-style 'split-window)

;; 再定義。なぜかfletでエラーになる
(defun helm-descbinds-sort-sections (sections)
  (cl-flet ((order (x)
                   (loop for n = 0 then (1+ n)
                         for regexp in helm-descbinds-section-order
                         if (and (car x) (string-match regexp (car x))) return n
                         finally return n)))
    (sort sections (lambda (a b)
                     (< (order a) (order b))))))

;; helm-goto-lineを使うと上手くいかないので再定義
(defun helm-goto-line (lineno &optional noanim)
  (helm-log-run-hook 'helm-goto-line-before-hook)
  (goto-char (point-min))
  (goto-char (point-at-bol lineno)))

;; face
(custom-set-faces
 '(helm-selection ((t (:background "DarkGreen" :underline t))))
 '(helm-ff-file ((t (:inherit default))))
 '(helm-ff-directory ((t (:inherit dired-directory))))
 ;; '(helm-ff-executable ((t (:inherit default))))
 ;; '(helm-ff-symlink ((t (:inherit default))))
 ;; '(helm-ff-invalid-symlink ((t (:inherit default))))
 )

;; helm-for-filesにバッファ一覧は不要
(setq helm-for-files-preferred-list
      (delq 'helm-source-buffers-list helm-for-files-preferred-list))

;; helm-find-fileで入力にマッチする候補が一つになった時点で自動補完はしない
(setq helm-ff-auto-update-initial-value nil)

;; helm動作中のキーバインド
(defvar ~helm-modify-keymap-required nil)
(defvar ~helm-modify-keymap-finished nil)

(defadvice helm-get-candidate-number (before mod-keymap activate)
  (when (and ~helm-modify-keymap-required
             (not ~helm-modify-keymap-finished))
    (when (boundp 'e2wm:prefix-key)
      (define-key helm-map (kbd e2wm:prefix-key) nil))
    (define-key helm-map (kbd "C-j")   'helm-next-line)
    (define-key helm-map (kbd "C-k")   'helm-previous-line)
    (define-key helm-map (kbd "C-h")   'helm-previous-source)
    (define-key helm-map (kbd "C-l")   'helm-next-source)
    (define-key helm-map (kbd "C-S-j") 'helm-next-page)
    (define-key helm-map (kbd "C-S-k") 'helm-previous-page)
    (define-key helm-map (kbd "C-{")   'helm-beginning-of-buffer)
    (define-key helm-map (kbd "C-}")   'helm-end-of-buffer)
    ;; (define-key helm-map (kbd "C-f")   'helm-delete-minibuffer-contents)
    (setq ~helm-modify-keymap-finished t)))

(defadvice helm-read-pattern-maybe (around mod-keymap activate)
  (let ((~helm-modify-keymap-required t)
        (~helm-modify-keymap-finished nil))
    ad-do-it))

(defun ~helm-multi-occur-to-helm-candidates ()
  (interactive)
  (let ((f (lambda (buffers)
             (let ((query (read-string "Regexp: ")))
               (multi-occur buffers query)))))
    (with-helm-alive-p
      (helm-run-after-exit f (helm-marked-candidates)))))

(define-key helm-buffer-map (kbd "C-o")     'helm-buffer-switch-other-window)
(define-key helm-buffer-map (kbd "C-S-c s") 'helm-buffer-run-grep)
(define-key helm-buffer-map (kbd "C-S-c S") 'helm-buffer-run-zgrep)
(define-key helm-buffer-map (kbd "C-S-c e") 'helm-buffer-run-ediff)
(define-key helm-buffer-map (kbd "C-S-c E") 'helm-buffer-run-ediff-merge)
(define-key helm-buffer-map (kbd "C-S-c u") 'helm-buffer-revert-persistent)
(define-key helm-buffer-map (kbd "C-S-c d") 'helm-buffer-run-kill-buffers)
(define-key helm-buffer-map (kbd "C-S-c r") 'helm-buffer-run-query-replace)
(define-key helm-buffer-map (kbd "C-S-c m") 'helm-toggle-all-marks)
(define-key helm-buffer-map (kbd "C-S-c a") 'helm-mark-all)
(define-key helm-buffer-map (kbd "C-S-c t") 'helm-buffers-toggle-show-hidden-buffers)
(define-key helm-buffer-map (kbd "C-S-c o") 'helm-buffers-run-multi-occur)
(define-key helm-buffer-map (kbd "C-S-c O") '~helm-multi-occur-to-helm-candidates)

(define-key helm-generic-files-map (kbd "C-o")     'helm-ff-run-switch-other-window)
(define-key helm-generic-files-map (kbd "C-S-c s") 'helm-ff-run-grep)
(define-key helm-generic-files-map (kbd "C-S-c S") 'helm-ff-run-zgrep)
(define-key helm-generic-files-map (kbd "C-S-c o") 'helm-ff-run-switch-other-window)
(define-key helm-generic-files-map (kbd "C-S-c e") 'helm-ff-run-ediff-file)
(define-key helm-generic-files-map (kbd "C-S-c E") 'helm-ff-run-ediff-merge-file)
(define-key helm-generic-files-map (kbd "C-S-c d") 'helm-ff-run-delete-file)
(define-key helm-generic-files-map (kbd "C-S-c c") 'helm-ff-run-copy-file)
(define-key helm-generic-files-map (kbd "C-S-c C") 'helm-ff-run-symlink-file)
(define-key helm-generic-files-map (kbd "C-S-c r") 'helm-ff-run-rename-file)
(define-key helm-generic-files-map (kbd "C-S-c p") 'helm-ff-run-print-file)

(define-key helm-find-files-map (kbd "C-o")     'helm-ff-run-switch-other-window)
(define-key helm-find-files-map (kbd "C-S-c s") 'helm-ff-run-grep)
(define-key helm-find-files-map (kbd "C-S-c S") 'helm-ff-run-zgrep)
(define-key helm-find-files-map (kbd "C-S-c o") 'helm-ff-run-switch-other-window)
(define-key helm-find-files-map (kbd "C-S-c e") 'helm-ff-run-ediff-file)
(define-key helm-find-files-map (kbd "C-S-c E") 'helm-ff-run-ediff-merge-file)
(define-key helm-find-files-map (kbd "C-S-c d") 'helm-ff-run-delete-file)
(define-key helm-find-files-map (kbd "C-S-c c") 'helm-ff-run-copy-file)
(define-key helm-find-files-map (kbd "C-S-c C") 'helm-ff-run-symlink-file)
(define-key helm-find-files-map (kbd "C-S-c r") 'helm-ff-run-rename-file)
(define-key helm-find-files-map (kbd "C-S-c p") 'helm-ff-run-print-file)
(define-key helm-find-files-map (kbd "C-S-h")   'helm-find-files-up-one-level)
(define-key helm-find-files-map (kbd "C-S-l")   'helm-execute-persistent-action)

(define-key helm-read-file-map (kbd "C-l")   nil)
(define-key helm-read-file-map (kbd "C-S-h") 'helm-find-files-up-one-level)
(define-key helm-read-file-map (kbd "C-S-l") 'helm-execute-persistent-action)

;; Migemoを使う
(helm-migemo-mode 1)

;; helm-find-fileでデフォルトでカレントディレクトリを開くようにする
(defadvice helm-find-files-1 (before ~disable-preselect activate)
  (ad-set-arg 1 nil))

;; imenuの候補表示を調整
(setq helm-imenu-delimiter ": ")
(defadvice helm-imenu-transformer (after ~shortify activate)
  (let ((re (concat "\\`\\([A-Z]\\)[a-z]+" helm-imenu-delimiter)))
    (dolist (c ad-return-value)
      (setcar c (replace-regexp-in-string re (concat "\\1" helm-imenu-delimiter) (car c))))))

;; バッファ一覧の候補表示を調整
(setq helm-buffer-max-length 60)
(defun helm-highlight-buffers (buffers _source)
  (cl-loop for i in buffers
           for (name size mode meta) = (if helm-buffer-details-flag
                                           (helm-buffer--details i 'details)
                                         (helm-buffer--details i))
           for truncbuf = (if (> (string-width name) helm-buffer-max-length)
                              (helm-substring-by-width
                               name helm-buffer-max-length)
                            (concat name (make-string
                                          (- (+ helm-buffer-max-length 3)
                                             (string-width name)) ? )))
           collect (cons (if helm-buffer-details-flag
                             (concat truncbuf "  " meta)
                           name)
                         (get-buffer i))))

;; p-r

;; bufがnilの場合がある
(defun helm-buffers--match-from-inside (candidate)
  (let* ((cand (replace-regexp-in-string "^\\s-\\{1\\}" "" candidate))
         (buf  (get-buffer cand))
         (regexp (cl-loop with pattern = helm-pattern
                          for p in (split-string pattern)
                          when (string-match "\\`@\\(.*\\)" p)
                          return (match-string 1 p))))
    (if (and regexp buf)
        (with-current-buffer buf
          (save-excursion
            (goto-char (point-min))
            (re-search-forward regexp nil t)))
        t)))
