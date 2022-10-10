(bundle pophint :type github :pkgname "aki2o/emacs-pophint")
(use-package pophint
  :defer t
  :custom ((pophint:popup-chars "hjklyuiopnmgfdatre;")
           (pophint:popup-max-tips 1000)
           (pophint:switch-direction-p nil)
           (pophint:select-source-method 'use-popup-char)
           ;; (pophint:select-source-chars "tregfdbvc")
           (pophint:switch-source-selectors '(("Quoted"   . "q")
                                              ("Url/Path" . "u")
                                              ("Cmt"      . "c")
                                              ("Line"     . "l")
                                              ("Sym"      . "s")))
           (pophint-mark:yank-immediately-on-marking-p nil)
           (pophint-isearch:start-on-isearch-exit-p nil)
           (pophint-yank:relayout-on-start-rangeyank-p nil)
           (pophint-eww:use-new-tab t)
           (pophint-w3m:use-new-tab t)
           (pophint-e2wm:array-quit-immediately t))
  
  :init
  (custom-set-faces
   '(pophint:tip-face     ((t (:background "HotPink4" :foreground "white" :bold t))))
   '(pophint:match-face   ((t (:background "dark slate gray" :foreground "white"))))
   '(pophint:pos-tip-face ((t (:background "black" :foreground "white")))))

  :config
  (pophint:set-allwindow-command pophint:do-flexibly)
  (pophint:set-allwindow-command pophint:do-rangeyank)

  ;; (bind-keys* ("M-y"   . pophint:do-flexibly-yank)
  ;;             ("C-M-y" . pophint:do-rangeyank)
  ;;             ("C-M-h" . pophint-region:backward)
  ;;             ("C-M-l" . pophint-region:forward)
  ;;             ("H-f"   . pophint-region:delete)
  ;;             ("H-d"   . pophint-region:backward-delete))
  ;;             ("C-H-f" . pophint-region:kill)
  ;;             ("C-H-d" . pophint-region:backward-kill))

  (define-key dired-mode-map (kbd ";") 'pophint:do-dired-node)

  (add-hook 'Info-mode-hook
            '(lambda () (local-set-key (kbd ";") 'pophint:do-info-ref))
            t)

  (add-hook 'help-mode-hook
            '(lambda () (local-set-key (kbd ";") 'pophint:do-help-btn))
            t)

  (add-hook 'Custom-mode-hook
            '(lambda () (local-set-key (kbd ";") 'pophint:do-widget))
            t)

  (pophint-tags:advice-command find-tag)
  (pophint-isearch:replace-to-yank-region isearch-yank-line)
  (pophint-isearch:replace-to-yank-region migemo-isearch-yank-line)

  (pophint-thing:advice-thing-at-point-function ~dwim-at-point)

  (pophint-tags:advice-command ~xref-find-references)
  (pophint-tags:advice-command ~xref-find-definitions)

  ;; For p-r

  (defun popup-delete (popup)
    "Delete POPUP instance."
    (when (popup-live-p popup)
      (popup-hide popup)
      (mapc 'delete-overlay (popup-overlays popup))
      (setf (popup-overlays popup) nil)
      (setq popup-instances (delq popup popup-instances))
      ;; Restore newlines state
      (let ((newlines (popup-newlines popup)))
        (when (> newlines 0)
          (popup-save-buffer-state
            (goto-char (point-max))
            (dotimes (i newlines)
              (if (and (char-before)
                       (= (char-before) ?\n))
                  (delete-char -1)))))))
    nil))

(defun ~pophint:forward ()
  (interactive)
  (pophint:do :direction 'forward))

(defun ~pophint:backward ()
  (interactive)
  (pophint:do :direction 'backward))

