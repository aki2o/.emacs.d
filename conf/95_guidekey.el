(use-package guide-key
  :config
  (setq guide-key/polling-time 3.0)
  (setq guide-key/idle-delay 1.0)
  (setq guide-key/guide-key-sequence
        '("<f1>" "C-x RET" "C-x ESC" "C-x 4" "C-x 5" "C-x 6"
          "C-x n" "C-x r" "C-x v" "C-x p"
          "C-x h"
          "C-x C-t" "C-x C-k" "C-x C-r"
          "C-o" "C-c" "C-S-c"
          "M-m"
          "C-M-p"))
  (guide-key-mode 1)
  
  (defmacro* guide-key/setup-local-keystroke (mode &key kbd not-highlight)
    (let ((forms (loop for keystroke in (if (stringp (eval kbd))
                                            (list (eval kbd))
                                          (eval kbd))
                       collect `(guide-key/add-local-guide-key-sequence ,keystroke)))
          (hook (intern-soft (concat (symbol-name (eval mode)) "-hook")))
          (func (intern (concat "guide-key/setup-keystroke-for-" (symbol-name (eval mode)))))
          (re (when (not not-highlight)
                (concat "\\`"
                        (replace-regexp-in-string "-mode\\'" "" (symbol-name (eval mode)))
                        "[:/\\-][a-zA-Z0-9]"))))
      (when (symbolp hook)
        `(progn
           (defun ,func ()
             ,@forms
             (when (stringp ,re)
               (guide-key/add-local-highlight-command-regexp ,re)))
           (add-hook ',hook ',func t)))))

  (defmacro guide-key/setup-local-highlight (mode &rest regexp-list)
    (let ((forms (loop for re in regexp-list
                       collect `(guide-key/add-local-highlight-command-regexp ,re)))
          (hook (intern-soft (concat (symbol-name (eval mode)) "-hook")))
          (func (intern (concat "guide-key/setup-highlight-for-" (symbol-name (eval mode))))))
      (when (symbolp hook)
        `(progn
           (defun ,func ()
             ,@forms)
           (add-hook ',hook ',func t)))))

  (when (boundp 'e2wm:prefix-key)
    (add-to-list 'guide-key/guide-key-sequence
                 (replace-regexp-in-string " +\\'" "" e2wm:prefix-key)))

  (when (fboundp 'org-mode)
    (guide-key/setup-local-keystroke 'org-mode :kbd "C-c C-x")
    (guide-key/setup-local-highlight 'org-mode "\\`outline-"))

  (when (fboundp 'eww-mode)
    (guide-key/setup-local-keystroke 'eww-mode :kbd ";")))


(use-package guide-key-tip
  :after (guide-key)
  :defer t
  :config
  (setq guide-key-tip/enabled t)

  (custom-set-faces
   '(guide-key-tip/pos-tip-face ((t (:foreground "white" :background "black"))))))
