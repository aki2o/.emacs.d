(bundle tail)
(use-package tail
  :defer t
  :bind* (("C-x C-t"   . nil)
          ("C-x C-t f" . tail-file)
          ("C-x C-t c" . tail-command))
  :init
  (setq tail-hide-delay 5)
  (setq tail-max-size 50)
  (setq tail-volatile 'nil) ;表示するタイミングで古い出力を消すかどうか
  (setq tail-raise t))


(use-package autorevert
  :defer t
  :commands (auto-revert-tail-mode)
  :hook (find-file . ~auto-revert-tail-mode-on)
  :config
  (add-hook 'after-revert-hook
            '(lambda ()
               (when auto-revert-mode (end-of-buffer)))
            t)
  )

(defun ~auto-revert-tail-mode-on ()
  (interactive)
  (when (string-match "/log/" default-directory)
    (auto-revert-tail-mode t)))

