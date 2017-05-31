;;;;;;;;;
;; Ido

;; (bundle ido-completing-read+)
;; (bundle ido-ubiquitous)
;; (bundle ido-vertical-mode)
;; (bundle flx-ido)
;; (bundle smex)

;; (require 'ido)

;; (setq ido-enable-flex-matching t)
;; (setq ido-create-new-buffer 'always)
;; (setq ido-save-directory-list-file (concat user-emacs-directory "cache/ido.last"))
;; (setq ido-max-window-height 0.75)
;; (setq ido-auto-merge-work-directories-length nil) ; 別フォルダの同名ファイルを自動補完するクソ機能を止める
;; (setq confirm-nonexistent-file-or-buffer t)

;; (defun ido-init-completion-maps ()
;;   ;; Common map
;;   (let ((map (make-sparse-keymap)))
;;     (define-key map "\C-a"          'ido-toggle-ignore)
;;     (define-key map "\C-e"          'ido-edit-input)
;;     (define-key map "\t"            'ido-complete)
;;     (define-key map " "             'ido-complete-space)
;;     (define-key map (kbd "C-S-l")   'ido-exit-minibuffer)
;;     (define-key map "\C-j"          'ido-next-match)
;;     (define-key map "\C-k"          'ido-prev-match)
;;     (define-key map (kbd "C-S-c c") 'ido-toggle-case)
;;     (define-key map (kbd "C-S-c m") 'ido-toggle-regexp)
;;     (define-key map (kbd "C-S-c p") 'ido-toggle-prefix)
;;     (define-key map (kbd "C-S-c u") 'ido-undo-merge-work-directory)
;;     (define-key map (kbd "C-S-c r") 'ido-restrict-to-matches)
;;     (define-key map (kbd "C-S-c 1") 'ido-take-first-match)
;;     (define-key map [right]         'ido-next-match)
;;     (define-key map [left]          'ido-prev-match)
;;     (define-key map "?"             'ido-completion-help)
;;     (define-key map "\C-h"          'ido-magic-backward-char)
;;     (define-key map "\C-l"          'ido-magic-forward-char)
;;     (define-key map "\C-f"          'ido-magic-delete-char)
;;     (set-keymap-parent map minibuffer-local-map)
;;     (setq ido-common-completion-map map))
;;   ;; File and directory map
;;   (let ((map (make-sparse-keymap)))
;;     (define-key map [remap switch-to-buffer] 'ido-enter-switch-buffer)
;;     (define-key map [remap find-file]        'ido-fallback-command)
;;     (define-key map [remap dired]            'ido-enter-dired)
;;     (define-key map [down]                   'ido-next-match-dir)
;;     (define-key map [up]                     'ido-prev-match-dir)
;;     (define-key map [(meta up)]              'ido-prev-work-directory)
;;     (define-key map [(meta down)]            'ido-next-work-directory)
;;     (define-key map (kbd "C-S-h")            'ido-delete-backward-word-updir)
;;     (define-key map (kbd "C-S-c ^")          'ido-up-directory)
;;     (define-key map (kbd "C-S-c g")          'ido-reread-directory)
;;     (define-key map (kbd "C-S-c w")          'ido-wide-find-dir-or-delete-dir)
;;     (define-key map (kbd "C-S-c W")          'ido-wide-find-file-or-pop-dir)
;;     (define-key map (kbd "C-S-c a")          'ido-push-dir)
;;     (define-key map (kbd "C-S-c A")          'ido-push-dir-first)
;;     (define-key map (kbd "C-S-c f")          'ido-forget-work-directory)
;;     (define-key map (kbd "C-S-c n")          'ido-prev-work-file)
;;     (define-key map (kbd "C-S-c p")          'ido-next-work-file)
;;     (define-key map (kbd "C-S-c N")          'ido-next-work-directory)
;;     (define-key map (kbd "C-S-c P")          'ido-prev-work-directory)
;;     (define-key map (kbd "C-S-c m")          'ido-make-directory)
;;     (define-key map (kbd "C-S-c M")          'ido-merge-work-directories)
;;     (set-keymap-parent map ido-common-completion-map)
;;     (setq ido-file-dir-completion-map map))
;;   ;; File only map
;;   (let ((map (make-sparse-keymap)))
;;     (define-key map (kbd "C-S-c k")   'ido-delete-file-at-head)
;;     (define-key map (kbd "C-S-c c w") 'ido-copy-current-word)
;;     (define-key map (kbd "C-S-c c f") 'ido-copy-current-file-name)
;;     (define-key map (kbd "C-S-c l")   'ido-toggle-literal)
;;     (set-keymap-parent map ido-file-dir-completion-map)
;;     (setq ido-file-completion-map map))
;;   ;; Buffer map
;;   (let ((map (make-sparse-keymap)))
;;     (define-key map [remap find-file]        'ido-enter-find-file)
;;     (define-key map [remap switch-to-buffer] 'ido-fallback-command)
;;     (define-key map (kbd "C-S-c k")          'ido-kill-buffer-at-head)
;;     (define-key map (kbd "C-S-c v")          'ido-toggle-virtual-buffers)
;;     (set-keymap-parent map ido-common-completion-map)
;;     (setq ido-buffer-completion-map map)))

;; (ido-mode 1)
;; ;; (ido-mode 'buffers)
;; ;; (ido-everywhere 1)

;; ;; (defun ~ido-recentf ()
;; ;;   (interactive)
;; ;;   (if (find-file (ido-completing-read "Find recent file: " recentf-list))
;; ;;       (message "Opening file...")
;; ;;     (message "Aborting")))

;; ;; (bind-keys* ("C-x f" . ~ido-recentf))

;; (use-package ido-vertical-mode
;;   :init
;;   (ido-vertical-mode 1))

;; ;; ;; ファイル開くと、全部read-onlyになってしまうので、一旦コメントアウト
;; ;; ;; (use-package ido-ubiquitous
;; ;; ;;   :init
;; ;; ;;   (ido-ubiquitous-mode 1))

;; (use-package flx-ido
;;   :init
;;   (setq flx-ido-threshold 10000)
  
;;   (flx-ido-mode 1))

;; ;; (use-package smex
;; ;;   :bind (("M-x" . smex))
  
;; ;;   :init
;; ;;   (setq smex-save-file (concat user-emacs-directory "cache/.smex-items"))
  
;; ;;   (smex-initialize))


;;;;;;;;;
;; Ivy

(bundle ivy)
(require 'ivy)

(setq ivy-height 15)
(setq ivy-fixed-height-minibuffer t)
(setq ivy-use-virtual-buffers t)

(setq ivy-minibuffer-map
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "C-m")   'ivy-done)
        (define-key map (kbd "C-z")   'ivy-call)
        (define-key map (kbd "C-g")   'minibuffer-keyboard-quit)
        (define-key map (kbd "SPC")   'self-insert-command)
        
        (define-key map (kbd "C-j")   'ivy-next-line)
        (define-key map (kbd "C-k")   'ivy-previous-line)
        (define-key map (kbd "C-S-j") 'ivy-scroll-up-command)
        (define-key map (kbd "C-S-k") 'ivy-scroll-down-command)
        (define-key map (kbd "C-S-{") 'ivy-beginning-of-buffer)
        (define-key map (kbd "C-S-}") 'ivy-end-of-buffer)
        
        (define-key map (kbd "TAB")   'ivy-partial-or-done)
        (define-key map (kbd "C-S-l") 'ivy-partial-or-done)
        (define-key map (kbd "C-l")   'ivy-forward-char)
        (define-key map (kbd "C-d")   'ivy-backward-delete-char)
        (define-key map (kbd "C-S-h") 'ivy-backward-kill-word)
        
        (define-key map (kbd "C-f")   'ivy-delete-char)
        (define-key map (kbd "H-f")   'ivy-kill-word)
        (define-key map (kbd "C-S-f") 'ivy-kill-line)
        (define-key map (kbd "C-w")   'ivy-kill-ring-save)
        
        (define-key map (kbd "C-S-c q") 'ivy-alt-done)
        (define-key map (kbd "C-S-c Q") 'ivy-immediate-done)
        (define-key map (kbd "C-S-c n") 'ivy-next-line-or-history)
        (define-key map (kbd "C-S-c s") 'ivy-reverse-i-search)
        (define-key map (kbd "C-S-c h") 'ivy-next-history-element)
        (define-key map (kbd "C-S-c H") 'ivy-previous-history-element)
        (define-key map (kbd "C-S-c c") 'ivy-next-line-and-call)
        (define-key map (kbd "C-S-c C") 'ivy-previous-line-and-call)
        (define-key map (kbd "C-S-c m") 'ivy-toggle-regexp-quote)
        (define-key map (kbd "C-S-c y") 'ivy-yank-word)
        (define-key map (kbd "C-S-c I") 'ivy-insert-current)
        (define-key map (kbd "C-S-c b") 'hydra-ivy/body)
        (define-key map (kbd "C-S-c d") 'ivy-dispatching-done)
        (define-key map (kbd "C-S-c D") 'ivy-dispatching-call)
        (define-key map (kbd "C-S-c r") 'ivy-restrict-to-matches)
        (define-key map (kbd "C-S-c a") 'ivy-avy)
        (define-key map (kbd "C-S-c R") 'ivy-read-action)
        (define-key map (kbd "C-S-c o") 'ivy-occur)
        (define-key map (kbd "C-S-c i") 'ivy-toggle-ignore)
        (define-key map (kbd "C-S-c ?") 'ivy-help)
        map))

(custom-set-faces
 '(ivy-current-match ((t (:background "ForestGreen" :distant-foreground "black")))))

(delete (assoc 'counsel-M-x ivy-initial-inputs-alist) ivy-initial-inputs-alist)

;; (ivy-mode 1)
;; switch-to-buffer を置き換えたくないので、ivy-modeの他の処理だけ手動で実行
(setq completing-read-function 'ivy-completing-read)
(when ivy-do-completion-in-region
  (setq completion-in-region-function 'ivy-completion-in-region))

(bundle counsel)
(require 'counsel)

;; (global-set-key [remap execute-extended-command] 'counsel-M-x)
;; (global-set-key [remap find-file]                'counsel-find-file)
;; (global-set-key [remap yank-pop]                 'counsel-yank-pop)
;; (global-set-key (kbd "M-i")                      'counsel-imenu)

(setq counsel-git-grep-map
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "C-p")     'counsel-git-grep-recenter)
        (define-key map (kbd "C-S-c r") 'counsel-git-grep-query-replace)
        (define-key map (kbd "C-S-c W") 'counsel-git-grep-switch-cmd)
        map))

(setq counsel-ag-map
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "C-p")     'counsel-git-grep-recenter)
        (define-key map (kbd "C-S-c r") 'counsel-git-grep-query-replace)
        map))

;; (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)

;; p-r

;; no set ivy-height

(defun counsel-yank-pop ()
  "Ivy replacement for `yank-pop'."
  (interactive)
  (if (eq last-command 'yank)
      (progn
        (setq ivy-completion-end (point))
        (setq ivy-completion-beg
              (save-excursion
                (search-backward (car kill-ring))
                (point))))
    (setq ivy-completion-beg (point))
    (setq ivy-completion-end (point)))
  (let ((candidates (cl-remove-if
                     (lambda (s)
                       (or (< (length s) 3)
                           (string-match "\\`[\n[:blank:]]+\\'" s)))
                     (delete-dups kill-ring))))
    (let ((ivy-format-function #'counsel--yank-pop-format-function))
      (ivy-read "kill-ring: " candidates
                :action 'counsel-yank-pop-action
                :caller 'counsel-yank-pop))))

(defun counsel-git-log ()
  "Call the \"git log --grep\" shell command."
  (interactive)
  (let ((counsel-async-split-string-re "\ncommit ")
        (counsel-yank-pop-truncate-radius 5)
        (ivy-format-function #'counsel--yank-pop-format-function))
    (ivy-read "Grep log: " #'counsel-git-log-function
              :dynamic-collection t
              :action #'counsel-git-log-action
              :unwind #'counsel-delete-process
              :caller 'counsel-git-log)))

;; マイナーモードが優先されてしまう

(defadvice ivy--minibuffer-setup (before ~override-minor-mode activate)
  (setq overriding-local-map ivy-minibuffer-map))

;; windowの復帰をaction側に任せてるっぽい

;; (defun ivy-call ()
;;   "Call the current action without exiting completion."
;;   (interactive)
;;   (unless ivy-inhibit-action
;;     (let ((action (ivy--get-action ivy-last)))
;;       (when action
;;         (let* ((collection (ivy-state-collection ivy-last))
;;                (x (cond ((and (consp collection)
;;                               (consp (car collection))
;;                               (cdr (assoc ivy--current collection))))
;;                         ((equal ivy--current "")
;;                          ivy-text)
;;                         (t
;;                          ivy--current))))
;;           (with-selected-window (or (active-minibuffer-window)
;;                                     (get-buffer-window))
;;             (save-selected-window
;;               (funcall action x))))))))

;; actionでは復帰とかしない

(defun counsel-git-grep-action (x)
  (when (string-match "\\`\\(.*?\\):\\([0-9]+\\):\\(.*\\)\\'" x)
    (let ((file-name (match-string-no-properties 1 x))
          (line-number (match-string-no-properties 2 x)))
      (find-file (expand-file-name file-name counsel--git-grep-dir))
      (goto-char (point-min))
      (forward-line (1- (string-to-number line-number)))
      (re-search-forward (ivy--regex ivy-text t) (line-end-position) t)
      (setf (ivy-state-window ivy-last) (get-buffer-window))
      (unless (eq ivy-exit 'done)
        (swiper--cleanup)
        (swiper--add-overlays (ivy--regex ivy-text))))))

;; windowを考慮できてない

(defun swiper--cleanup ()
  "Clean up the overlays."
  (while swiper--overlays
    (let ((ov (pop swiper--overlays)))
      (with-selected-window (overlay-get ov 'window)
        (save-excursion
          (goto-char (point-min))
          (isearch-clean-overlays)))
      (delete-overlay ov))))


;;;;;;;;;;
;; Helm

(bundle helm)
(bundle helm-migemo)
(bundle helm-descbinds)

(require 'helm)
(require 'helm-config)
(require 'helm-files)
(require 'helm-migemo)

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

;; (global-set-key [remap apropos-command]          'helm-apropos)
;; 有効にしなくても、勝手に置き換わる
;; さらに、これを有効にすると、大文字を含むファイル名を新規作成できなくなる
;; (global-set-key (kbd "C-x C-f")                  'helm-find-files)

(require 'helm-descbinds) ; replace from descbinds-anything
(helm-descbinds-install)  ; (descbinds-anything-install)
;; ;; descbinds-anythingも他のanythingコマンドと同じように
(setq helm-descbinds-window-style 'split-window) ; (setq descbinds-anything-window-style 'split-window)

;; 再定義。なぜかfletでエラーになる
(defun helm-descbinds-sort-sections (sections)
  (flet ((order (x)
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

(define-key helm-buffer-map (kbd "C-o")     'helm-buffer-switch-other-window)
(define-key helm-buffer-map (kbd "C-S-c s") 'helm-buffer-run-grep)
(define-key helm-buffer-map (kbd "C-S-c S") 'helm-buffer-run-zgrep)
(define-key helm-buffer-map (kbd "C-S-c o") 'helm-buffer-switch-other-window)
(define-key helm-buffer-map (kbd "C-S-c e") 'helm-buffer-run-ediff)
(define-key helm-buffer-map (kbd "C-S-c E") 'helm-buffer-run-ediff-merge)
(define-key helm-buffer-map (kbd "C-S-c u") 'helm-buffer-revert-persistent)
(define-key helm-buffer-map (kbd "C-S-c d") 'helm-buffer-run-kill-buffers)
(define-key helm-buffer-map (kbd "C-S-c r") 'helm-buffer-run-query-replace)
(define-key helm-buffer-map (kbd "C-S-c m") 'helm-toggle-all-marks)
(define-key helm-buffer-map (kbd "C-S-c a") 'helm-mark-all)
(define-key helm-buffer-map (kbd "C-S-c t") 'helm-buffers-toggle-show-hidden-buffers)

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

;; Migemoを使うコマンド
(helm-migemize-command helm-find-files)
(helm-migemize-command helm-for-files)
;; アップデートしたらエラーになったのでとりあえず無効
;; (helm-migemize-command helm-buffers-list)
(helm-migemize-command helm-show-kill-ring)

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


;;;;;;;;;;;;;;
;; Anything

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


;; (global-set-key [remap find-file]        'anything-find-file)
;; (global-set-key (kbd "C-x f")            'anything-recentf)
;; (global-set-key [remap switch-to-buffer] 'anything-buffers-list)
;; (global-set-key [remap yank-pop]         'anything-show-kill-ring)

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
  :defer t)

