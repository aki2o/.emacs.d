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

  (define-key ibuffer-mode-map (kbd "C-k") nil) ; キーバインドを変更しない
  (define-key ibuffer-mode-map (kbd "M-s") nil) ; グローバルマップを使う

  )


(bundle emacswiki:lcomp)
(use-package lcomp
  :config
  (lcomp-install))

