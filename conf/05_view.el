(use-package view
  :defer t
  :config
  (setq view-read-only t)

  (defvar ~view-next-activate nil)
  (defun ~view-toggle-next-activate ()
    (interactive)
    (message "View mode be active automatically : %s"
             (setq ~view-next-activate (not ~view-next-activate))))

  (add-hook 'find-file-hook '~view-chk-next-activate t)
  (defun ~view-chk-next-activate ()
    (when (and ~view-next-activate
               (buffer-file-name))
      (read-only-mode 1)))

  (defun ~view-scroll-up ()
    (interactive)
    (View-scroll-line-forward 10))

  (defun ~view-scroll-down ()
    (interactive)
    (View-scroll-line-backward 10))

  (defvar ~view-mode-shortcut-keys
    '("C-h" "C-j" "C-k" "C-l" "C-a" "C-e" "C-{" "C-}" "C-<" "C->"
      "C-S-h" "C-S-j" "C-S-k" "C-S-l" "C-S-a" "C-S-e"
      "C-s" "C-S-s" "C-p"))

  (add-hook 'view-mode-hook '~view-mode-setup t)
  (defvar ~view-mode-bind-customize-finish (make-hash-table))
  (defun ~view-mode-setup ()
    (when (not (gethash major-mode ~view-mode-bind-customize-finish))
      (with-temp-buffer
        (cl-loop for key in ~view-mode-shortcut-keys
                 for cmd = (key-binding (kbd key) t)
                 for len = (length key)
                 for char = (substring key (1- len) len)
                 for shift-char = (when (string-match "S-.\\'" key)
                                    (upcase char))
                 do (define-key view-mode-map (kbd (or shift-char char)) cmd)))
      (puthash major-mode t ~view-mode-bind-customize-finish)))

  (bind-keys :map view-mode-map
             ("C-j"   . next-line)
             ("C-k"   . previous-line)
             ("C-S-j" . ~view-scroll-up)
             ("C-S-k" . ~view-scroll-down)))


(bundle viewer :type github :pkgname "rubikitch/viewer")
(use-package viewer
  :defer t
  :config
  ;; view-modeのときはモードラインの色を変える
  (setq viewer-modeline-color-unwritable "indian red")
  (setq viewer-modeline-color-view "hot pink")
  (viewer-change-modeline-color-setup)

  ;; view-modeで開くファイル
  (setq view-mode-by-default-regexp
        (rx-to-string `(or (and "." (or "log" "gz") eos)
                           (and "/.rbenv/")
                           ;; (and bos ,(expand-file-name (concat user-emacs-directory "elisp/")))
                           (and "/vendor/bundle/"))))
  
  ;; 書き込めないファイルは常にview-mode
  (viewer-stay-in-setup)

  ;; view-modeを抜けても戻らなくなってしまったのでコメントアウト
  ;; ;; キーバインドをモード間で統一
  ;; (define-overriding-view-mode-map emacs-lisp-mode ("RET" . find-function-at-point))
  ;; (define-overriding-view-mode-map c-mode ("RET" . gtags-find-tag-from-here))

  (defadvice package-menu-execute (around ~disable-view-setting activate)
    (let ((view-mode-by-default-regexp nil))
      ad-do-it)))
