(bundle emacswiki:tail)
(use-package tail
  :bind* (("C-x C-t"   . nil)
          ("C-x C-t f" . tail-file)
          ("C-x C-t c" . tail-command))
  
  :init
  
  (setq tail-hide-delay 5)
  (setq tail-max-size 50)
  (setq tail-volatile 'nil) ;表示するタイミングで古い出力を消すかどうか
  (setq tail-raise t)
  
  )


(use-package autorevert
  :commands (auto-revert-tail-mode)

  :init
  
  (add-hook 'find-file-hook '~auto-revert-tail-mode-on t)
  
  (defun ~auto-revert-tail-mode-on ()
    (interactive)
    (when (string-match "/log/" default-directory)
      (auto-revert-tail-mode t)))
  
  (add-hook 'after-revert-hook
            '(lambda ()
               (when auto-revert-tail-mode (end-of-buffer)))
            t)
  
  )


;;http://d.hatena.ne.jp/kitokitoki/20100706/p1
(make-face '~highlight-face)
(set-face-foreground '~highlight-face "black")
(set-face-background '~highlight-face "yellow")
(setq ~highlight-face '~highlight-face)

(defun ~keep-highlight-regexp (re)
  (interactive "sRegexp: \n")
  (setq ~highlight-keyword re)
  (~keep-highlight-set-font-lock ~highlight-keyword))

(defun ~keep-highlight-symbole-at-point ()
  (interactive)
  (setq ~highlight-keyword (or (thing-at-point 'symbol) ""))
  (~keep-highlight-set-font-lock ~highlight-keyword))

(defun ~keep-highlight-set-font-lock (re)
  (font-lock-add-keywords 'nil (list (list re 0 ~highlight-face t)))
  (font-lock-fontify-buffer))
 
(defun ~cancel-highlight-regexp ()
  (interactive)
  (font-lock-remove-keywords 'nil (list (list ~highlight-keyword 0 ~highlight-face t)))
  (font-lock-fontify-buffer))

