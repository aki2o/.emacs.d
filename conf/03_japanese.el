;;; 日本語環境設定
(set-language-environment "Japanese")
(set-default-coding-systems 'utf-8-unix)
;; (prefer-coding-system 'utf-8-unix)
;; (set-terminal-coding-system 'euc-japan)
(set-buffer-file-coding-system 'utf-8-unix)

(cond
 ((~is-windows)
  ;; Meadow
  ;; (mw32-ime-initialize)
  ;; (setq default-input-method "MW32-IME")
  ;; (setq mw32-ime-show-mode-line t)
  ;; (setq-default mw32-ime-mode-line-state-indicator "[--]")
  ;; (setq mw32-ime-mode-line-state-indicator-list '("[--]" "[あ]" "[--]"))
  ;; (add-hook 'mw32-ime-on-hook
  ;;           (function (lambda () (set-cursor-color "red"))))
  ;; (add-hook 'mw32-ime-off-hook
  ;;           (function (lambda () (set-cursor-color "black"))))
  ;; NTEmacs
  (setq default-input-method "W32-IME")
  (w32-ime-initialize)
  (setq-default w32-ime-mode-line-state-indicator "[--]")
  (setq w32-ime-mode-line-state-indicator-list '("[--]" "[あ]" "[--]")))
 ((~is-mac)
  (setq default-input-method "MacOSX")
  (mac-set-input-method-parameter "com.google.inputmethod.Japanese.base" 'title "あ")
  ;; minibuffer 内は英数モードにする
  (add-hook 'minibuffer-setup-hook 'mac-change-language-to-us t)
  ;; backslash を優先
  (mac-translate-from-yen-to-backslash))
 (t
  (bundle mozc)
  (use-package mozc
    :config
    (setq default-input-method "japanese-mozc")
    (setq mozc-candidate-style 'overlay))
  ;; mini buffer ではオフに
  (add-hook 'minibuffer-setup-hook 'ibus-disable)))

(add-hook 'input-method-activate-hook (lambda() (set-cursor-color "red")) t)
(add-hook 'input-method-inactivate-hook (lambda() (set-cursor-color "white")) t)

(defun ~deactivate-input-method ()
  (interactive)
  (when current-input-method
    (deactivate-input-method)))

(defun ~activate-input-method ()
  (interactive)
  (when (not current-input-method)
    (activate-input-method
     (or (car input-method-history) default-input-method))))

(bind-key* "<muhenkan>" '~deactivate-input-method)
(bind-key* "<henkan>" '~activate-input-method)


;; 全角スペースの表示
(setq whitespace-style '(tabs spaces space-mark))
(setq whitespace-space-regexp "\\( +\\|\u3000+\\)")
(setq whitespace-display-mappings '((space-mark ?\u3000 [?\u25a1])))
(global-whitespace-mode 1)


;; ;; jaspace
;; ;; (bundle jaspace :url "http://homepage3.nifty.com/satomii/software/jaspace.el")
;; ;; タブ, 全角スペース、改行直前の半角スペースを表示する
;; (when (require 'jaspace nil t)
;;   (when (boundp 'jaspace-modes)
;;     (setq jaspace-modes (append jaspace-modes
;;                                 (list 'php-mode
;;                                       'yaml-mode
;;                                       'javascript-mode
;;                                       'ruby-mode
;;                                       'text-mode
;;                                       'perl-mode
;;                                       'nxml-mode
;;                                       'fundamental-mode))))
;;   (when (boundp 'jaspace-alternate-jaspace-string)
;;     (setq jaspace-alternate-jaspace-string "□"))
;;   (when (boundp 'jaspace-highlight-tabs)
;;     (setq jaspace-highlight-tabs ?^))
;;   (add-hook 'jaspace-mode-off-hook
;;             (lambda()
;;               (when (boundp 'show-trailing-whitespace)
;;                 (setq show-trailing-whitespace nil))))
;;   (add-hook 'jaspace-mode-hook
;;             (lambda()
;;               (progn
;;                 (when (boundp 'show-trailing-whitespace)
;;                   (setq show-trailing-whitespace t))
;;                 (face-spec-set 'jaspace-highlight-jaspace-face
;;                                '((((class color) (background light))
;;                                   (:foreground "blue"))
;;                                  (t (:foreground "green"))))
;;                 (face-spec-set 'jaspace-highlight-tab-face
;;                                '((((class color) (background light))
;;                                   (:foreground "red"
;;                                    :background "unspecified"
;;                                    :strike-through nil
;;                                    :underline t))
;;                                  (t (:foreground "purple"
;;                                      :background "unspecified"
;;                                      :strike-through nil
;;                                      :underline t))))
;;                 (face-spec-set 'trailing-whitespace
;;                                '((((class color) (background light))
;;                                   (:foreground "red"
;;                                    :background "unspecified"
;;                                    :strike-through nil
;;                                    :underline t))
;;                                  (t (:foreground "purple"
;;                                      :background "unspecified"
;;                                      :strike-through nil
;;                                      :underline t))))))))



