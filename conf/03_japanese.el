;;; 日本語環境設定
(set-language-environment "Japanese")
(set-default-coding-systems 'utf-8-unix)
;; (prefer-coding-system 'utf-8-unix)
;; (set-terminal-coding-system 'euc-japan)
(set-buffer-file-coding-system 'utf-8-unix)

(setenv "LC_ALL" "ja_JP.UTF-8")

(defun ~activate-input-method ()
  (interactive)
  (when (not current-input-method)
    (activate-input-method
     ;; (or (car input-method-history) default-input-method))
     default-input-method)))

(defun ~deactivate-input-method ()
  (interactive)
  (when current-input-method
    (deactivate-input-method)))

(bind-key* "<henkan>" '~activate-input-method)
(bind-key* "<muhenkan>" '~deactivate-input-method)

;; (add-hook 'input-method-activate-hook (lambda() (set-cursor-color "red")) t)
;; (add-hook 'input-method-inactivate-hook (lambda() (set-cursor-color "white")) t)

;; 全角スペースの表示
(setq whitespace-style '(tabs spaces space-mark))
(setq whitespace-space-regexp "\\( +\\|\u3000+\\)")
(setq whitespace-display-mappings '((space-mark ?\u3000 [?\u25a1])))
(global-whitespace-mode 1)


;; OSのIMEが起動しているとEmacsのIMEが正常に動かないので，Emacsを利用中はOSのIMEを無効化する
(when (~is-mac)
  (defun ~send-eisuu-key ()
    "Emulating alphanumeric keys"
    (interactive)
    (call-process "osascript" nil t nil "-e" "tell application \"System Events\" to key code 102"))
  (add-hook 'focus-in-hook '~send-eisuu-key))


;; Mozc
(use-package mozc
  :defer t
  :custom ((mozc-leim-title "[あ]")
           (mozc-helper-program-name "mozc_emacs_helper"))
  :hook (mozc-mode . ~send-eisuu-key)
  :init
  (setq default-input-method "japanese-mozc")
  :config
  (with-eval-after-load 'helm
    (define-key mozc-mode-map (kbd "M-x") 'helm-M-x)
    (add-hook 'helm-minibuffer-set-up-hook '~deactivate-input-method t)))


(use-package mozc-popup
  :if window-system
  :after mozc
  :init
  (use-package popup :defer t)
  :config
  (setq mozc-candidate-style 'popup))


(use-package mozc-cursor-color
  :after mozc
  :config
  (setq mozc-cursor-color-alist
        '((direct . "white")
          (read-only . "hot pink")
          (hiragana . "cyan")
          (full-katakana . "cyan")
          (half-ascii . "cyan")
          (full-ascii . "cyan")
          (half-katakana . "cyan"))))
