(defalias 'perl-mode 'cperl-mode)
(use-package cperl-mode
  :defer t
  :custom ((cperl-indent-level 4)
           (cperl-continued-statement-offset 0)
           (cperl-brace-offset 0)
           (cperl-label-offset 0)
           (cperl-indent-parens-as-block t)
           (cperl-close-paren-offset 0)
           (cperl-tab-always-indent t)
           ;; (cperl-electric-parens t) ; 対応する括弧自動挿入 うざい
           ;; (cperl-invalid-face nil)
           (cperl-highlight-variables-indiscriminately t))
  
  :init
  (with-eval-after-load 'mmask
    (mmask-regist-extension-with-icase 'perl-mode "cgi"))
  
  :config
  (set-face-foreground 'cperl-array-face "PeachPuff2")
  (set-face-background 'cperl-array-face (face-background 'default))
  (set-face-foreground 'cperl-hash-face "LightSalmon")
  (set-face-background 'cperl-hash-face (face-background 'default))

  (~add-setup-hook 'cperl-mode
    (setq indent-tabs-mode nil)
    ;; BestPractices からぱくったがなんかうごいてない
    (setq fill-column 78)
    (setq auto-fill-mode t)
    
    (local-set-key (kbd "C-c h") 'cperl-perldoc)
    (local-set-key (kbd "C-j") nil)

    (setq ~tidy-code-current-function '~perltidy-apply))

  (~add-setup-hook-after-load 'mmask 'cperl-mode
    (setq moccur-grep-default-mask (mmask-get-regexp-string 'perl-mode))))
  
(defun ~perltidy-apply ()
  (interactive)
  (save-excursion (shell-command-on-region (point-min) (point-max) "perltidy -q" nil t)))


;; (use-package set-perl5lib
;;   :after (cperl-mode)
;;   :config
;;   ;; set-perl5lib のデフォルトだとパス内にある最上位のlibフォルダが対象になってしまうので、
;;   ;; 最下位のlibフォルダを対象にするように変更
;;   (defadvice perllib-check-path (around perllib-check-path-reverse activate)
;;     (let* ((lst (ad-get-arg 0))
;;            (rlst (if (listp lst) (reverse lst) (list ""))))
;;       (loop while (> (length rlst) 0)
;;             for node = (car rlst)
;;             if (string= node "lib") return nil
;;             if (string= node "t") return nil
;;             do (setq rlst (cdr rlst))
;;             (setq ad-return-value (cond ((and (listp rlst)
;;                                               (> (length rlst) 0))
;;                                          (concat "/" (mapconcat 'identity (reverse rlst) "/")))
;;                                         (t
;;                                          "")))))))


;; (bundle plsense)
;; (use-package plsense
;;   :after (cperl-mode)
;;   :custom ((plsense-popup-help-key "C-'")
;;            (plsense-display-help-buffer-key "C-\"")
;;            (plsense-jump-to-definition-key "C->")
;;            ;; (plsense-server-start-automatically-p t)
;;            ;; (plsense-plcmp-candidate-foreground-color nil)
;;            )

;;   :config
;;   (plsense-config-default)

;;   (with-eval-after-load 'pophint-autoloads
;;     (pophint-config:set-tag-jump-command plsense-jump-to-definition)))


;; (bundle plsense-direx)
;; (use-package plsense-direx
;;   :after (plsense)
;;   :custom ((plsense-direx:open-explorer-other-window-key "C-x d")
;;            (plsense-direx:open-referer-other-window-key "C-x D"))
;;   :config
;;   (plsense-direx:config-default))


;; (bundle e2wm-pkgex4pl)
;; (use-package e2wm-pkgex4pl
;;   :after (cperl-mode))
