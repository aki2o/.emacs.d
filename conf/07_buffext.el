(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquify-ignore-buffers-re "\s*\*[^\*]+\*\s*")


;; (bundle ibuffer :url "http://www.bookshelf.jp/elc/ibuffer.el")
(use-package ibuffer
  :config
  
  (defun ~ibuffer-other-window ()
    (interactive)
    (when (one-window-p t)
      (split-window-horizontally))
    (other-window 1)
    (ibuffer))

  (global-set-key [remap list-buffers] '~ibuffer-other-window)

  (define-key ibuffer-mode-map (kbd "C-k") nil) ; キーバインドを変更しない
  (define-key ibuffer-mode-map (kbd "M-s") nil) ; グローバルマップを使う

  )


(bundle lcomp :url "http://www.geocities.co.jp/SiliconValley-Oakland/7673/lisp/lcomp.el")
(use-package lcomp
  :config
  (lcomp-install))


;; (bundle auto-save-buffers)
;; (use-package auto-save-buffers
;;   :defer t
;;   :init
;;   (setq auto-save-buffers-active-p nil)
;;   :config
;;   (run-with-idle-timer 5 t 'auto-save-buffers))


;; (require 'auto-save-buffers-enhanced)
;; (setq auto-save-buffers-enhanced-use-svk-flag t)
;; (auto-save-buffers-enhanced-include-only-checkout-path t)
;; (auto-save-buffers-enhanced t)
;; (global-set-key "\C-xas" 'auto-save-buffers-enhanced-toggle-activity)


