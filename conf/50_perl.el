(defalias 'perl-mode 'cperl-mode)

(use-package cperl-mode
  :defer t
  :init
  ;; CGIもPerlのファイルとして開く
  (mmask-regist-extension-with-icase 'perl-mode "cgi")
  
  (setq cperl-indent-level 4)
  (setq cperl-continued-statement-offset 0)
  (setq cperl-brace-offset 0)
  (setq cperl-label-offset 0)
  (setq cperl-indent-parens-as-block t)
  (setq cperl-close-paren-offset 0)
  (setq cperl-tab-always-indent t)
  ;; (setq cperl-electric-parens t) ; 対応する括弧自動挿入 うざい
  ;; (setq cperl-invalid-face nil)
  (setq cperl-highlight-variables-indiscriminately t)

  :config
  (add-hook 'cperl-mode-hook '~perl-mode-setup t)

  (defun ~perl-mode-setup ()
    ;; steal from perlhacks
    (setq indent-tabs-mode nil)
    ;; BestPractices からぱくったがなんかうごいてない
    (setq fill-column 78)
    (setq auto-fill-mode t)
    (set-face-foreground 'cperl-array-face "PeachPuff2")
    (set-face-background 'cperl-array-face (face-background 'default))
    (set-face-foreground 'cperl-hash-face "LightSalmon")
    (set-face-background 'cperl-hash-face (face-background 'default))
    
    (local-set-key (kbd "C-c h") 'cperl-perldoc)
    (local-set-key (kbd "C-j") nil)
    
    ;; perl tidy
    (local-set-key (kbd "C-c t r") 'perltidy-region)
    (local-set-key (kbd "C-c t d") 'perltidy-defun)
    
    ;; auto-complete
    ;; デフォルトの情報源に同モード内単語をいれないようにする
    (setq ac-sources (delq 'ac-source-words-in-same-mode-buffers ac-sources))
    (add-to-list 'ac-sources 'ac-source-yasnippet)
  
    ;; color-moccur
    (when (fboundp 'mmask-get-regexp-string)
      (setq moccur-grep-default-mask (mmask-get-regexp-string 'perl-mode)))

    (when (fbondp 'flycheck-mode)
      (flycheck-mode 1)))
  
  (defun perltidy-region ()
    (interactive)
    (save-excursion (shell-command-on-region (point) (mark) "perltidy -q" nil t)))
  
  (defun perltidy-defun ()
    (interactive)
    (save-excursion (mark-defun) (perltidy-region))))


(use-package plsense
  :defer t
  :after (cperl-mode)
  :config
  (setq plsense-popup-help-key "C-'")
  (setq plsense-display-help-buffer-key "C-\"")
  (setq plsense-jump-to-definition-key "C->")
  ;; (setq plsense-server-start-automatically-p t)
  ;; (setq plsense-plcmp-candidate-foreground-color nil)
  (plsense-config-default)
    
  (when (fboundp 'pophint-tags:advice-command)
    (pophint-tags:advice-command plsense-jump-to-definition)))
  

(use-package plsense-direx
  :defer t
  :after (:all cperl-mode plsense)
  :config
  (setq plsense-direx:open-explorer-other-window-key "C-x d")
  (setq plsense-direx:open-referer-other-window-key "C-x D")
  (plsense-direx:config-default))


(use-package e2wm-pkgex4pl
  :defer t
  :after (e2wm))
