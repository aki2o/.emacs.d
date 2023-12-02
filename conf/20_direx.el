(use-package direx
  :defer t
  :functions (direx:direx-mode)
  :init
  (setq direx:leaf-icon "  "
        direx:open-icon "▾ "
        direx:closed-icon "▸ ")

  :config
  (require 'direx-project)

  (define-key direx:direx-mode-map (kbd "C-j") nil)
  (define-key direx:direx-mode-map (kbd "C-k") nil)
  (define-key direx:direx-mode-map (kbd "C-h") nil)
  (define-key direx:direx-mode-map (kbd "C-l") nil)
  (define-key direx:direx-mode-map (kbd "j") 'direx:next-item)
  (define-key direx:direx-mode-map (kbd "k") 'direx:previous-item)
  (define-key direx:direx-mode-map (kbd "h") 'direx:up-item)
  (define-key direx:direx-mode-map (kbd "l") 'direx:down-item)
  (define-key direx:direx-mode-map (kbd "J") 'direx:next-sibling-item)
  (define-key direx:direx-mode-map (kbd "K") 'direx:previous-sibling-item)
  (define-key direx:direx-mode-map (kbd "RET") 'direx:find-item)
  (define-key direx:direx-mode-map (kbd "C-RET") 'direx:find-item-other-window)

  (with-eval-after-load 'projectile
    (setq direx-project:project-root-predicate-functions
          '((lambda (dir)
              (loop for e in (append projectile-project-root-files
                                     projectile-project-root-files-bottom-up)
                    thereis (file-exists-p (expand-file-name e dir))))))))

(defun ~direx:jump-to-smartly ()
  (interactive)
  (cond (current-prefix-arg
         (dired-jump))
        (t
         (or (ignore-errors
               (direx-project:jump-to-project-root-other-window)
               t)
             (direx:jump-to-directory-other-window)))))


(use-package direx-grep
  :after (direx)
  :config
  (define-key direx:direx-mode-map (kbd "s") 'direx-grep:grep-item-from-root)
  (define-key direx:direx-mode-map (kbd "S") 'direx-grep:grep-item)
  (define-key direx:direx-mode-map (kbd "a") 'direx-grep:show-all-item)
  (define-key direx:direx-mode-map (kbd "A") 'direx-grep:show-all-item-at-point))


(use-package e2wm-direx
  :after (e2wm))
