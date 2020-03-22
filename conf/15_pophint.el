(bundle pophint)
(use-package pophint
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
           (pophint-region:kill-ring-save-p nil)
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
              ("C-M-h" . pophint-region:backward)
              ("C-M-l" . pophint-region:forward)
              ("H-f"   . pophint-region:kill)
              ("H-d"   . pophint-region:backward-kill))

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

  (with-eval-after-load 'ag
    (pophint-thing:advice-thing-at-point-function ag/dwim-at-point)
    (pophint-thing:defcommand-noadvice ~ag))

  (with-eval-after-load 'helm-ag
    (pophint-thing:advice-thing-at-point-function helm-ag--insert-thing-at-point)
    (pophint-thing:defcommand-noadvice ~helm-ag))

  (with-eval-after-load 'counsel
    (pophint-thing:advice-thing-at-point-function ~counsel-initial-input))

  (with-eval-after-load 'e2wm
    (e2wm:add-keymap
     e2wm:pst-minor-mode-keymap
     '(("prefix ;" . pophint:do-situationally-e2wm)
       ("M-;"      . pophint:do-situationally-e2wm)
       ) e2wm:prefix-key)

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
                            (e2wm:pst-window-select-main))))))


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
