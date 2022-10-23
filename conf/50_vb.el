;; auto-complete.el が有効になっちゃうので一旦コメントアウト
;; (bundle visual-basic-mode)
;; (use-package visual-basic-mode
;;   :if (~is-windows)
;;   :defer t
;;   :commands (visual-basic-mode)
;;   :custom ((visual-basic-mode-indent 4)
;;            ;; (visual-basic-ide-pathname "H:/VS98/VB98/VB6.EXE")
;;            )
;;   :init
;;   (with-eval-after-load 'mmask
;;     (mmask-regist-extension-with-icase 'visual-basic-mode "vb" "vbs" "cls" "bas" "frm" "rvb"))

;;   :config
;;   (~add-setup-hook-after-load 'mmask 'visual-basic-mode
;;     (setq moccur-grep-default-mask (mmask-get-regexp-string 'visual-basic-mode))))


;; (bundle vbasense)
;; (use-package vbasense
;;   :after (visual-basic-mode)
;;   :custom ((vbasense-popup-help-key "C-'")
;;            (vbasense-jump-to-definition-key "C->")
;;            (vbasense-setup-user-library-function '(lambda ()
;;                                                     (vbasense-load-directory "x:/MyLib/vba")
;;                                                     (vbasense-load-project))))
;;   :config
;;   (add-to-list 'vbasense-tli-files "c:/Program Files/Common Files/System/ado/msado21.tlb")
;;   (add-to-list 'vbasense-tli-files "c:/Program Files/Common Files/Microsoft Shared/DAO/dao360.dll")
;;   (vbasense-config-default)

;;   (with-eval-after-load 'pophint-autoloads
;;     (pophint-tags:advice-command vbasense-jump-to-definition)))

