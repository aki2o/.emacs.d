(bundle consult :type git :url "git@github.com:aki2o/consult.git" :branch "fix_jump_preview")
(use-package consult
  :custom ((consult-narrow-key (kbd "C-n"))
           (consult-preview-key (kbd "C-M-;"))
           (consult-preview-raw-size 1048576))

  :bind (([remap apropos-command] . consult-apropos)
         ([remap pop-global-mark] . consult-global-mark)
         ([remap recentf-open-files] . consult-recent-file)
         :map ~keyjack-mode-map
         ([remap pop-global-mark] . consult-global-mark)
         ([remap recentf-open-files] . consult-recent-file)
         :map isearch-mode-map
         ("C-M-p" . consult-isearch-history)
         ("M-c" . consult-line)
         :map minibuffer-local-map
         ([remap next-matching-history-element] . consult-history)
         ([remap previous-matching-history-element] . consult-history))

  :hook (completion-list-mode . consult-preview-at-point-mode)

  :init
  (setq register-preview-function 'consult-register-format)
  (advice-add 'register-preview :override 'consult-register-window)

  (setq xref-show-xrefs-function 'consult-xref)
  (setq xref-show-definitions-function 'consult-xref)

  :config
  (define-key consult-narrow-map (vconcat consult-narrow-key "?") 'consult-narrow-help)

  (setq consult-after-jump-hook '(~consult-after-jump))

  (with-eval-after-load 'pophint
    (pophint-thing:defcommand-noadvice ~consult-grep)
    (pophint-thing:defcommand-noadvice ~consult-git-grep)
    (pophint-thing:defcommand-noadvice ~consult-ripgrep))
  )

(defun ~consult-after-jump ()
  (recenter)
  (lexical-let* ((vbeg (save-excursion (beginning-of-visual-line) (point)))
                 (vend (save-excursion (end-of-visual-line) (point)))
                 (end (line-end-position))
                 (ov (make-overlay vbeg (if (= vend end) (1+ end) vend))))
    (overlay-put ov 'face 'highlight)
    (run-with-idle-timer 1 nil (lambda () (when ov (delete-overlay ov))))))

(defun ~consult-grep (dir)
  (interactive
   (list (read-directory-name "Dir: ")))
  (funcall 'consult-grep dir (~dwim-at-point)))

(defun ~consult-git-grep (&optional dir)
  (interactive)
  (funcall 'consult-git-grep dir (~dwim-at-point)))

(defun ~consult-ripgrep (dir)
  (interactive
   (list (read-directory-name "Dir: ")))
  (funcall 'consult-ripgrep dir (~dwim-at-point)))

