(with-eval-after-load 'mmask
  (mmask-regist-name-regexp 'sh-mode (rx-to-string `(and bos (? ".") (or "env" "envrc") (* anything)))))

(setq sh-basic-offset 4)
(setq sh-indentation 4)

(add-hook 'sh-mode-hook '~sh-mode-setup t)

(defun ~sh-mode-setup ()
  (when (find-library-name "mmask")
    (setq moccur-grep-default-mask (mmask-get-regexp-string 'sh-mode)))

  (when (find-library-name "flex-autopair")
    (setq flex-autopair-default-conditions '(((and (eq major-mode 'sh-mode)
                                                   (eq last-command-event ?\[))
                                              . self)
                                             ((and (eq major-mode 'sh-mode)
                                                   (eq last-command-event ?\())
                                              . self)))
    (flex-autopair-reload-conditions)))

