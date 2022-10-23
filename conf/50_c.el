;; (bundle cc-mode)
;; (use-package cc-mode
;;   :defer t
;;   :init
;;   (~add-setup-hook 'c-mode-common
;;     (c-set-style "k&r")
;;     (c-set-offset 'substatement-open 0)
;;     (c-set-offset 'case-label '+)
;;     (c-set-offset 'arglist-cont-nonempty '+)
;;     (c-set-offset 'arglist-intro '+)
;;     (c-set-offset 'topmost-intro-cont '+)
;;     (c-set-offset 'arglist-close 0)
;;     (setq tab-width 4)
;;     (setq c-basic-offset tab-width)
;;     (setq indent-tabs-mode nil)
;;     (setq c-auto-newline t)
;;     (setq c-tab-always-indent t))

;;   :config
;;   (~add-setup-hook-after-load 'mmask 'cc-mode
;;     (setq moccur-grep-default-mask (mmask-get-regexp-string 'cc-mode)))

;;   (~add-setup-hook-after-load 'flycheck 'cc-mode
;;     (flycheck-mode 1)))


;; (bundle c-eldoc)
;; (use-package c-eldoc
;;   :after (cc-mode)
;;   :config
;;   (~add-setup-hook 'c-mode-common
;;     (set (make-local-variable 'eldoc-idle-delay) 0.20)
;;     (c-turn-on-eldoc-mode)))
