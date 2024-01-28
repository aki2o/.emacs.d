(with-eval-after-load 'view
  (setq view-read-only t)

  (bind-keys :map view-mode-map
             ("C-j"   . next-line)
             ("C-k"   . previous-line)
             ("C-S-j" . ~view-scroll-up)
             ("C-S-k" . ~view-scroll-down))

  (~add-setup-hook 'view-mode
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

  (add-hook 'find-file-hook '~view-chk-next-activate t))

(defvar ~view-mode-bind-customize-finish (make-hash-table))

(defvar ~view-mode-shortcut-keys
  '("C-h" "C-j" "C-k" "C-l" "C-a" "C-e" "C-{" "C-}" "C-<" "C->"
    "C-S-h" "C-S-j" "C-S-k" "C-S-l" "C-S-a" "C-S-e"
    "C-s" "C-S-s" "C-p"))

(defvar ~view-next-activate nil)

(defun ~view-scroll-up ()
  (interactive)
  (View-scroll-line-forward 10))

(defun ~view-scroll-down ()
  (interactive)
  (View-scroll-line-backward 10))

(defun ~view-toggle-next-activate ()
  (interactive)
  (message "View mode be active automatically : %s"
           (setq ~view-next-activate (not ~view-next-activate))))

(defun ~view-chk-next-activate ()
  (when (and ~view-next-activate
             (buffer-file-name))
    (read-only-mode 1)))


(use-package viewer
  :after (view)
  :custom ((viewer-modeline-color-unwritable "indian red")
           (viewer-modeline-color-view "hot pink")
           (view-mode-by-default-regexp (rx-to-string
                                         `(or (and "." (or "log" "gz") eos)
                                              (and "/gems/")
                                              (and "/node_modules/")
                                              (and bos ,(expand-file-name (concat user-emacs-directory "elisp/")))))))
  :config
  (viewer-change-modeline-color-setup)
  (viewer-stay-in-setup) ;; 書き込めないファイルは常にview-mode
  )
