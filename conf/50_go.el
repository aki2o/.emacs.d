(bundle go-mode)
(use-package go-mode
  :custom (gofmt-command "goimports") ; go get golang.org/x/tools/cmd/goimports
  :config
  (defun ~go-setup-mode ()
    (add-hook 'before-save-hook 'gofmt-before-save t t)
    (setq-local tab-width 2))
  
  (add-hook 'go-mode-hook '~go-setup-mode t)

  (defun ~godoc ()
    (interactive)
    (godoc (completing-read "Query: " (go-packages))))
  
  (define-key go-mode-map (kbd "C-'") '~godoc)
  (define-key go-mode-map (kbd "C->") 'godef-jump-other-window)

  (use-package pophint
    :config
    (pophint-tags:advice-command godef-jump-other-window :point-arg-index 0))

  (use-package color-moccur
    :init
    (defun ~go-setup-color-moccur ()
      (setq moccur-grep-default-mask (mmask-get-regexp-string 'go-mode)))
    (add-hook 'go-mode-hook '~go-setup-color-moccur t))

  (use-package lsp-mode
    :hook (go-mode . lsp))

  ;; p-r

  ;; ファイル内の定義位置に移動する際に、対象のバッファを指定できていなかった
  
  (defun godef--find-file-line-column (specifier other-window)
    "Given a file name in the format of `filename:line:column',
visit FILENAME and go to line LINE and column COLUMN."
    (if (not (string-match "\\(.+\\):\\([0-9]+\\):\\([0-9]+\\)" specifier))
        ;; We've only been given a directory name
        (funcall (if other-window #'find-file-other-window #'find-file) specifier)
      (let ((filename (match-string 1 specifier))
            (line (string-to-number (match-string 2 specifier)))
            (column (string-to-number (match-string 3 specifier))))
        (with-current-buffer (funcall (if other-window #'find-file-other-window #'find-file) filename)
          (go--goto-line line)
          (beginning-of-line)
          (forward-char (1- column))
          (if (buffer-modified-p)
              (message "Buffer is modified, file position might not have been correct"))))))

  )


(bundle gom-mode)
(use-package gom-mode
  :defer t)

