(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquify-ignore-buffers-re "\s*\*[^\*]+\*\s*")
(setq enable-recursive-minibuffers t) ;; ミニバッファを再帰的に呼び出せるようにする


(use-package ibuffer
  :defer t
  :config
  (define-key ibuffer-mode-map (kbd "C-k") nil) ; キーバインドを変更しない
  (define-key ibuffer-mode-map (kbd "M-s") nil) ; グローバルマップを使う
  )

(defun ~ibuffer-other-window ()
  (interactive)
  (when (one-window-p t)
    (split-window-horizontally))
  (other-window 1)
  (ibuffer))
