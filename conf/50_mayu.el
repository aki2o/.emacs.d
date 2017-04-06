(bundle byplayer/yamy :name mayu-mode)
(use-package mayu-mode

  :if (~is-windows)

  :init
  
  (mmask-regist-extension-with-icase 'mayu-mode "mayu")

  (defun mayu-create-helpinfo () "Create infomation of bind and help from config of mayu."
         (interactive)
         (when (string= (file-name-extension buffer-file-name) "mayu")
           (start-process-shell-command "mayu-create-helpinfo" nil (concat "mkmayuhelp " (shell-quote-argument default-directory)))))

  (add-hook 'after-save-hook 'mayu-create-helpinfo t)

  :config
  
  (add-hook 'mayu-mode-hook
            '(lambda () 
               (add-to-list 'align-rules-list '(mayu-key-value (regexp . "=") (modes . '(mayu-mode))))
               (add-to-list 'align-rules-list '(mayu-comment (regexp . "#") (modes . '(mayu-mode))))
               )
            t)

  (use-package color-moccur
    :config
    (add-hook 'mayu-mode-hook
              '(lambda ()
                 (setq moccur-grep-default-mask (mmask-get-regexp-string 'mayu-mode)))
              t))

  )

