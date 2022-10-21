(setq sh-basic-offset 4)
(setq sh-indentation 4)

(with-eval-after-load 'mmask
  (mmask-regist-name-regexp 'sh-mode (rx-to-string `(and bos (? ".") (or "env" "envrc") (* anything)))))

(~add-setup-hook-after-load 'mmask 'sh-mode
  (setq moccur-grep-default-mask (mmask-get-regexp-string 'sh-mode)))

(~add-setup-hook-after-load 'flex-autopair 'sh-mode
  (setq flex-autopair-default-conditions '(((and (eq major-mode 'sh-mode)
                                                 (eq last-command-event ?\[))
                                            . self)
                                           ((and (eq major-mode 'sh-mode)
                                                 (eq last-command-event ?\())
                                            . self)))
  (flex-autopair-reload-conditions))

