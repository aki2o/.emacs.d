(bundle consult :type github :pkgname "minad/consult")
(use-package consult
  :bind (([remap apropos-command] . consult-apropos)
         ([remap imenu] . consult-imenu)
         ([remap pop-global-mark] . consult-global-mark)
         ([remap recentf-open-files] . consult-recent-file)
         :map ~keyjack-mode-map
         ([remap imenu] . consult-imenu)
         ([remap pop-global-mark] . consult-global-mark)
         ([remap recentf-open-files] . consult-recent-file)
         :map isearch-mode-map
              ("C-M-p" . consult-isearch-history)
              ("H-s" . consult-line)
         :map minibuffer-local-map
              ([remap next-matching-history-element] . consult-history)
              ([remap previous-matching-history-element] . consult-history))
  
  :hook (completion-list-mode . consult-preview-at-point-mode)

  :init
  (setq register-preview-function 'consult-register-format)
  (advice-add 'register-preview :override 'consult-register-window)

  (setq xref-show-xrefs-function 'consult-xref)
  (setq xref-show-definitions-function 'consult-xref)

  (with-eval-after-load 'projectile
    (setq consult-project-function (lambda (_) (projectile-project-root))))

  :config
  (consult-customize consult-theme
                     :preview-key
                     (list (kbd "C-M-;")
                           :debounce 0.1 (kbd "C-j") (kbd "C-k") (kbd "<up>") (kbd "<down>")
                           :debounce 0.5 'any))

  (with-eval-after-load 'pophint
    (pophint-thing:defcommand-noadvice ~consult-grep))
  )

(defun ~consult-grep (&optional dir)
  (interactive)
  (funcall 'consult-grep dir (~dwim-at-point)))

(defun ~consult-git-grep (&optional dir)
  (interactive)
  (funcall 'consult-git-grep dir (~dwim-at-point)))

(defun ~consult-ripgrep (&optional dir)
  (interactive)
  (funcall 'consult-ripgrep dir (~dwim-at-point)))

