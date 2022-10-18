(with-eval-after-load 'mmask
  (mmask-regist-extension-with-icase 'emacs-lisp-mode "el"))

(add-hook 'emacs-lisp-mode-hook '~emacs-lisp-mode-setup t)
(defun ~emacs-lisp-mode-setup ()
  (add-to-list '~browse-document-url-functions '~elisp-document-url-with)
  (setq ~find-definition-function '~find-tag-elisp)
  (setq ~popup-document-frame-function '~popup-tip-elisp-symbol-help)
  ;; flycheck
  (~disable-flycheck-in-emacs-conf-file-buffer)
  ;; color-moccur
  (when (featurep 'mmask)
    (setq moccur-grep-default-mask (mmask-get-regexp-string 'emacs-lisp-mode))))

(defun ~elisp-document-url-with (words)
  (concat "https://www.gnu.org/software/emacs/manual/html_mono/elisp.html"
          (if (> (length words) 0)
              (concat "#index-" (replace-regexp-in-string "-" "_002d" (nth 0 words)))
            "")))

(defun ~find-tag-elisp ()
  (interactive)
  (let* ((sym (symbol-at-point)))
    (cond ((functionp sym)
           (find-function sym))
          ((symbolp sym)
           (find-variable sym))
          (t
           (message "Can't specify symbol at point for find tag")))))

(with-eval-after-load 'pophint
  (pophint-tags:advice-command ~find-tag-elisp))

(defun ~popup-tip-elisp-symbol-help ()
  (interactive)
  (let* ((sym (symbol-at-point))
         (doc (ignore-errors
                (or (describe-function-1 sym)
                    (documentation-property sym 'variable-documentation)))))
    (when (not doc)
      (setq doc "** Can't specify symbol at point for popup help! **"))
    (pos-tip-show doc)))

(defun ~disable-flycheck-in-emacs-conf-file-buffer ()
  (let ((confdir (expand-file-name (concat user-emacs-directory "conf")))
        (bufpath (buffer-file-name)))
    (when (and bufpath
               (>= (length bufpath) (length confdir))
               (string= (substring bufpath 0 (length confdir))
                        confdir)
               (functionp 'flycheck-mode))
      (flycheck-mode 0))))


(bundle eldoc-extension)
(use-package eldoc-extension
  :defer t
  :init
  (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode t)
  (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode t)
  (add-hook 'ielm-mode-hook 'turn-on-eldoc-mode t)
  (setq eldoc-idle-delay 0.1)
  (setq eldoc-echo-area-use-multiline-p t)
  ;; (setq eldoc-minor-mode-string "")
  )


(use-package autodoc
  :defer t)


(bundle log4e)
(use-package log4e
  :defer t
  :init
  ;; (define-key emacs-lisp-mode-map (kbd "C-\\") 'log4e:insert-start-log-quickly)
  )
