(use-package auto-complete
  :defer t
  :init
  (global-auto-complete-mode 0)
  
  :config
  (ac-set-trigger-key "C-SPC") ;; 文字入力や削除時のみ補完開始するキー定義
  (setq ac-disable-faces nil) ;; コメントやリテラルでも自動補完
  (setq ac-use-menu-map t) ;; 補完メニュー表示時に特別なキーマップ（ac-menu-map）を有効にするかどうか
  (setq ac-stop-flymake-on-completing t) ;; 補完時にFlymakeを中止するかどうか

  ;; パフォーマンスに関わる設定
  (setq ac-auto-start 4) ;; 補完の自動開始入力文字数
  ;; (setq ac-delay 5) ;; 補完可能になるまでの遅延時間（秒）を実数で指定
  ;; (setq ac-auto-show-menu 0.5) ;; 自動で補完メニューを表示するまでの時間（秒）を実数で指定
  (setq ac-use-comphist t) ;; 補完推測機能を有効にするかどうか
  (setq ac-menu-height 20) ;; 補完メニューの高さ
  (setq ac-candidate-limit 50) ;; 補完候補数の制限

  (bind-keys :map ac-completing-map
             ("C-g" . ac-stop)
             ("C-n" . nil)
             ("C-p" . nil)
             ("C-j" . ac-next)
             ("C-k" . ac-previous))

  (bind-keys :map popup-menu-keymap
             ("C-j"   . popup-next)
             ("C-k"   . popup-previous)
             ("C-l"   . popup-open)
             ("C-h"   . popup-close)
             ("C-S-j" . popup-page-next)
             ("C-S-k" . popup-page-previous))
  
  (bind-keys :map popup-isearch-keymap
             ("C-h" . popup-isearch-close))

  ;; Face
  (set-face-background 'popup-scroll-bar-foreground-face "slate gray")

  ;; 日本語入力中にonだとうざいので切る
  (defadvice ac-handle-post-command (around ~disable-while-input-japanese activate)
    (or current-input-method ad-do-it))

  ;; global-auto-complete-modeの対象に追加
  (dolist (mode '(snippet-mode visual-basic-mode shell-script-mode mayu-mode
                               shell-mode eshell-mode term-mode minibuffer-inactive-mode
                               git-commit-mode))
    (add-to-list 'ac-modes mode t))

  ;; 入力しつつ補完開始するコマンドを定義
  (defun self-insert-with-ac-trigger-command (n)
    (interactive "p")
    (self-insert-command n) (ac-trigger-key-command n))

  ;; ミニバッファでも補完
  (defvar ~ac-sources-in-minibuffer-alist
    '((eval-expression . ac-emacs-lisp-mode-setup)
      ;; ivyとかを使うので不要
      ;; (describe-function . ac-emacs-lisp-mode-setup)
      ;; (describe-variable . ac-emacs-lisp-mode-setup)
      ))

  (add-hook 'minibuffer-setup-hook '~ac-setup-in-minibuffer t)

  (defun ~ac-setup-in-minibuffer ()
    (let ((v (assoc-default this-command ~ac-sources-in-minibuffer-alist)))
      (when v
        (cond ((functionp v) (funcall v))
              ((symbolp v)   (add-to-list 'ac-sources v t))
              ((listp v)     (setq ac-sources (append ac-sources v))))
        (auto-complete-mode 1)))))


(use-package auto-complete-config
  :straight auto-complete
  :defer t
  :after (auto-complete-mode)
  :hook ((emacs-lisp-mode . ac-emacs-lisp-mode-setup)
         (c-mode-common . ac-cc-mode-setup)
         (ruby-mode . ac-ruby-mode-setup)
         (css-mode . ac-css-mode-setup)
         (auto-complete-mode . ac-common-setup))
  :config
  (setq-default ac-sources '(ac-source-abbrev ac-source-dictionary ac-source-words-in-same-mode-buffers)))


(use-package company
  :defer t
  :bind (("C-M-i" . company-complete))
  :custom ((company-idle-delay 0.2)
           (company-tooltip-limit 20)
           (company-minimum-prefix-length 4)
           (company-selection-wrap-around t)
           (company-transformers '(company-sort-by-backend-importance))
           (completion-ignore-case t))
  :init
  (global-company-mode 1)
  
  :config
  (custom-set-faces
   '(company-tooltip                  ((t (:foreground "black" :background "lightgray"))))
   '(company-tooltip-common           ((t (:foreground "darkred" :background "lightgray"))))
   '(company-tooltip-selection        ((t (:foreground "white" :background "steelblue"))))
   '(company-tooltip-common-selection ((t (:foreground "darkred" :background "steelblue"))))
   '(company-scrollbar-fg             ((t (:background "slate gray"))))
   '(company-scrollbar-bg             ((t (:background "gray")))))

  (bind-keys :map company-active-map
             ("M-n"  . nil)
             ("M-p"  . nil)
             ("C-h"  . nil)
             ("C-w"  . nil)
             ("C-j"  . company-select-next)
             ("C-k"  . company-select-previous)
             ("TAB"  . company-complete-selection)
             ("C-s"  . company-filter-candidates)
             ("C->"  . company-show-location)
             ("C-\"" . company-show-doc-buffer))

  (bind-keys :map company-search-map
             ("C-j" . company-select-next)
             ("C-k" . company-select-previous))
  
  ;; override by company-complete
  (defun ~company-prior-setup ()
    (local-set-key (kbd "C-M-i") 'company-complete))

  (defvar ~company-prior-modes '(emacs-lisp-mode))

  (dolist (mode ~company-prior-modes)
    (let ((hook (intern-soft (format "%s-hook" mode))))
      (when hook
        (add-hook hook '~company-prior-setup t))))

  (add-to-list 'company-backends '~company-words-in-same-mode-buffers t)

  (defun ~company-words-in-same-mode-buffers (command &optional arg &rest ignored)
    (interactive (list 'interactive))
    (cl-case command
      (interactive (company-begin-backend '~company-words-in-same-mode-buffers))
      (init (~company-update-word-index))
      (prefix (company-grab-symbol))
      (candidates (cons :async `(lambda (callback) (funcall callback (~company-word-candidates ,arg ,(point))))))
      (annotation "W")))

  (defvar ~company-word-index nil)
  (defun ~company-update-word-index ()
    (dolist (buffer (buffer-list))
      (when (not (eq buffer (current-buffer)))
        (with-current-buffer buffer
          (unless (local-variable-p '~company-word-index)
            (make-local-variable '~company-word-index))
          (if (null ~company-word-index)
              (setq ~company-word-index (cons nil nil)))
          (when (and (not (car ~company-word-index))
                     (< (buffer-size) 1048576))
            (setq ~company-word-index
                  (cons t
                        (split-string (buffer-substring-no-properties (point-min) (point-max))
                                      "\\(?:^\\|\\_>\\).*?\\(?:\\_<\\|$\\)"))))))))

  (defvar ~company-candidates-limit nil)
  (defun ~company-word-candidates (prefix point)
    (cl-loop initially (~company-incremental-update-word-index prefix point)
             for buffer in (buffer-list)
             if (and (or (not (integerp ~company-candidates-limit))
                         (< (length candidates) ~company-candidates-limit))
                     (derived-mode-p (buffer-local-value 'major-mode buffer)))
             append (all-completions
                     prefix
                     (and (local-variable-p '~company-word-index buffer)
                          (cdr (buffer-local-value '~company-word-index buffer))))
             into candidates
             finally return (delete-dups candidates)))

  (defun ~company-incremental-update-word-index (prefix point)
    (unless (local-variable-p '~company-word-index)
      (make-local-variable '~company-word-index))
    (if (null ~company-word-index)
        (setq ~company-word-index (cons nil nil)))
    ;; Mark incomplete
    (if (car ~company-word-index)
        (setcar ~company-word-index nil))
    (let ((index (cdr ~company-word-index))
          (words (~company-candidate-words-in-buffer
                  point
                  prefix
                  (or (and (integerp ~company-candidates-limit) ~company-candidates-limit) 10))))
      (dolist (word words)
        (unless (member word index)
          (push word index)
          (setcdr ~company-word-index index)))))

  (defun ~company-candidate-words-in-buffer (point prefix limit)
    (let ((i 0)
          candidate
          candidates
          (regexp (concat "\\_<" (regexp-quote prefix) "\\(\\sw\\|\\s_\\)+\\_>")))
      (save-excursion
        ;; Search backward
        (goto-char point)
        (while (and (or (not (integerp limit)) (< i limit))
                    (re-search-backward regexp nil t))
          (setq candidate (match-string-no-properties 0))
          (unless (member candidate candidates)
            (push candidate candidates)
            (cl-incf i)))
        ;; Search backward
        (goto-char (+ point (length prefix)))
        (while (and (or (not (integerp limit)) (< i limit))
                    (re-search-forward regexp nil t))
          (setq candidate (match-string-no-properties 0))
          (unless (member candidate candidates)
            (push candidate candidates)
            (cl-incf i)))
        (nreverse candidates)))))


(use-package company-quickhelp
  :defer t
  :after (company)
  :custom ((company-quickhelp-delay 1.5))
  :config
  ;; company-quickhelp-mode-map なくなったぽい
  ;; (bind-keys :map company-quickhelp-mode-map
  ;;            ("M-h" . nil)
  ;;            ("C-'" . company-quickhelp-manual-begin))
  (company-quickhelp-mode 1))


;; エラーになってしまうのでコメントアウトしてる
;; (use-package company-statistics
;;   :defer t
;;   :after (company)
;;   :config
;;   (add-to-list 'company-transformers 'company-sort-by-statistics))


(use-package company-box
  :defer t
  :after (:all company all-the-icons)
  :custom ((company-box-icons-alist 'company-box-icons-all-the-icons)
           (company-box-doc-delay 1.5)
           (company-box-doc-frame-parameters '((foreground-color . "black")
                                               (internal-border-width . 10))))
  :hook (company-mode . company-box-mode)
  :config
  (custom-set-faces
   '(company-box-candidate ((t (:foreground "black"))))
   '(company-box-scrollbar ((t (:background "slate gray"))))))
