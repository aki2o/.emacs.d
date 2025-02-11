;;; 日本語環境設定
(set-language-environment "Japanese")
(set-default-coding-systems 'utf-8-unix)
;; (set-terminal-coding-system 'euc-japan)
(set-buffer-file-coding-system 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)

(setenv "LC_ALL" "ja_JP.UTF-8")

(defun ~activate-input-method ()
  (interactive)
  (when (not current-input-method)
    (activate-input-method (or (car input-method-history) default-input-method))))

(defun ~deactivate-input-method ()
  (interactive)
  (when current-input-method
    (deactivate-input-method)))

(bind-key* "<henkan>" '~activate-input-method)
(bind-key* "<muhenkan>" '~deactivate-input-method)

(add-hook 'input-method-activate-hook (lambda() (set-cursor-color "cyan")) t)
(add-hook 'input-method-deactivate-hook (lambda() (set-cursor-color "white")) t)

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


;; https://qiita.com/wakamenod/items/db3ca7d16df37f4768b7
(when (functionp 'mac-ime-toggle)
  (bind-key* [remap toggle-input-method] 'mac-ime-toggle))
