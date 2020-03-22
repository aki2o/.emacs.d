(bundle auto-complete)
(use-package auto-complete
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

(bundle emacswiki:popup-pos-tip)
;; (use-package popup-pos-tip
;;   :after (auto-complete-mode)
;;   :init
;;   (defadvice popup-tip (around popup-pos-tip-wrapper (string &rest args) activate)
;;     (if (eq window-system 'x)
;;         (apply 'popup-pos-tip string args)
;;       ad-do-it)))

(use-package auto-complete-config
  :after (auto-complete-mode)
  :hook ((emacs-lisp-mode . ac-emacs-lisp-mode-setup)
         (c-mode-common . ac-cc-mode-setup)
         (ruby-mode . ac-ruby-mode-setup)
         (css-mode . ac-css-mode-setup)
         (auto-complete-mode . ac-common-setup))
  :config
  (setq-default ac-sources '(ac-source-abbrev ac-source-dictionary ac-source-words-in-same-mode-buffers)))


(bundle company)
(use-package company
  :bind (("C-M-i" . company-complete))

  :custom ((company-idle-delay 0.2)
           (company-tooltip-limit 20)
           (company-minimum-prefix-length 4)
           (company-selection-wrap-around t)
           (company-transformers '(company-sort-by-backend-importance)))
  
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
        (add-hook hook '~company-prior-setup t)))))

(bundle company-quickhelp)
(use-package company-quickhelp
  :after (company)
  :custom ((company-quickhelp-delay 2))
  
  :config
  ;; company-quickhelp-mode-map なくなったぽい
  ;; (bind-keys :map company-quickhelp-mode-map
  ;;            ("M-h" . nil)
  ;;            ("C-'" . company-quickhelp-manual-begin))
  (company-quickhelp-mode 1))

;; エラーになってしまうのでコメントアウトしてる
;; (bundle company-statistics)
;; (use-package company-statistics
;;   :after (company)
;;   :config
;;   (add-to-list 'company-transformers 'company-sort-by-statistics))

(bundle company-box)
(use-package company-box
  :after (company all-the-icons)
  :custom ((company-box-icons-alist 'company-box-icons-all-the-icons)
           (company-box-doc-delay 2)
           (company-box-doc-frame-parameters '((foreground-color . "black")
                                               (internal-border-width . 10))))
  :hook (company-mode . company-box-mode)

  :config
  (custom-set-faces
   '(company-box-candidate ((t (:foreground "black"))))
   '(company-box-scrollbar ((t (:background "slate gray"))))))
