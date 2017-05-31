(bundle pophint :autoloads nil)
(use-package pophint-config

  :init

  (custom-set-faces
   '(pophint:tip-face     ((t (:background "HotPink4" :foreground "white" :bold t))))
   '(pophint:match-face   ((t (:background "dark slate gray" :foreground "white"))))
   '(pophint:pos-tip-face ((t (:background "black" :foreground "white")))))

  (setq pophint:popup-chars "hjklyuiopnmgfdatre;")
  (setq pophint-config:effect-default-activated t)
  (setq pophint-config:run-defcommand-exhaustively-after-load nil)
  (setq pophint:popup-max-tips 1000)
  (setq pophint:switch-direction-p nil)
  (setq pophint:select-source-method 'use-popup-char)
  ;; (setq pophint:select-source-chars "tregfdbvc")
  (setq pophint:switch-source-selectors '(("Quoted"   . "q")
                                          ("Url/Path" . "u")
                                          ("Cmt"      . "c")
                                          ("Line"     . "l")
                                          ("Sym"      . "s")))

  :config

  (defun ~pophint:do-other-windows ()
    (interactive)
    (let ((currbuf (current-buffer)))
      (pophint:do :source `((activebufferp . (lambda (b) (not (eql b currbuf))))
                            ,@pophint--default-source)
                  :allwindow t)))
  
  (pophint:set-allwindow-command pophint:do-flexibly)
  (pophint:set-allwindow-command pophint:do-rangeyank)

  (bind-keys* ("C-;"   . pophint:do)
              ("C-:"   . ~pophint:do-other-windows)
              ("C-M-;" . pophint:do-interactively)
              ("M-;"   . pophint:redo)
              ("M-y"   . pophint:do-flexibly-yank)
              ("C-M-y" . pophint:do-rangeyank)
              ("C-M-h" . pophint-config:backward-region)
              ("C-M-l" . pophint-config:forward-region)
              ("H-f"   . pophint-config:kill-region)
              ("H-d"   . pophint-config:backward-kill-region))

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

  (pophint-config:set-automatically-when-marking t)
  (pophint-config:set-yank-immediately-when-marking nil)
  (pophint-config:set-automatically-when-isearch nil)
  (pophint-config:set-do-when-other-window t)
  (pophint-config:set-relayout-when-rangeyank-start nil)
  (pophint-config:set-eww-use-new-tab t)
  (pophint-config:set-w3m-use-new-tab t)
  (pophint-config:set-goto-immediately-when-e2wm-array t)
  (pophint-config:set-automatically-when-e2wm-array t)
  (pophint-config:set-kill-region-kill-ring-save nil)
  ;; (pophint-config:set-mark-direction 'forward)
  (pophint-config:set-tag-jump-command find-tag)
  (pophint-config:set-isearch-yank-region-command isearch-yank-line)
  (pophint-config:set-isearch-yank-region-command migemo-isearch-yank-line)

  
  ;; 11_e2wm.elでカスタマイズしたので変更
  (pophint:defsource
    :name "e2wm-history2"
    :description "Entry in history list2 plugin of e2wm."
    :source '((dedicated . e2wm)
              (regexp . "^... +\\([^ ]+\\)")
              (requires . 1)
              (highlight . nil)
              (activebufferp . (lambda (b)
                                 (and (e2wm:managed-p)
                                      (eq (buffer-local-value 'major-mode b)
                                          'e2wm:def-plugin-history-list2-mode))))
              (action . (lambda (hint)
                          (select-window (pophint:hint-window hint))
                          (goto-char (pophint:hint-startpt hint))
                          (e2wm:def-plugin-history-list2-select-command)
                          (e2wm:pst-window-select-main)))))
  

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
    nil)

  )


(use-package owdriver
  :defer t
  :config
  (owdriver-define-command pophint:do t (pophint:do :not-switch-window t)))


(use-package e2wm
  :defer t
  :config
  (e2wm:add-keymap
   e2wm:pst-minor-mode-keymap
   '(("prefix ;" . pophint:do-situationally-e2wm)
     ("M-;"      . pophint:do-situationally-e2wm)
     ) e2wm:prefix-key))

