(bundle autodisass-java-bytecode)
(use-package autodisass-java-bytecode
  :defer t)


(bundle google-c-style)
(use-package google-c-style
  :defer t
  :commands (google-set-c-style))


;; (bundle meghanada)
;; (use-package meghanada
;;   :defer t
;;   :custom ((meghanada-server-remote-debug nil)
;;            (meghanada-javac-xlint "-Xlint:all,-processing"))
;;   :bind (:map meghanada-mode-map
;;               ("M-RET" . meghanada-local-variable)
;;               ("C->"   . meghanada-jump-declaration)
;;               ("C-<"   . meghanada-back-jump)
;;               ("M-z"   . ~hydra-meghanada/body))

;;   :config
;;   (~add-setup-hook 'meghanada-mode
;;     (setq indent-tabs-mode nil)
;;     (setq tab-width 4)
;;     (setq c-basic-offset 4)
;;     ;; (google-set-c-style)
;;     ;; (google-make-newline-indent)
;;     ;; (add-hook 'before-save-hook 'meghanada-code-beautify-before-save)
;;     )

;;   (with-eval-after-load 'hydra
;;     (defhydra ~hydra-meghanada (:hint nil :exit t :idle ~hydra-help-delay)
;;       "
;; ^Edit^                           ^Tast or Task^
;; ^^^^^^-------------------------------------------------------
;; _f_: meghanada-compile-file      _m_: meghanada-restart
;; _c_: meghanada-compile-project   _t_: meghanada-run-task
;; _o_: meghanada-optimize-import   _j_: meghanada-run-junit-test-case
;; _s_: meghanada-switch-test-case  _J_: meghanada-run-junit-class
;; _v_: meghanada-local-variable    _r_: meghanada-run-junit-recent
;; _q_: exit
;; "
;;       ("f" meghanada-compile-file)
;;       ("m" meghanada-restart)

;;       ("c" meghanada-compile-project)
;;       ("o" meghanada-optimize-import)
;;       ("s" meghanada-switch-test-case)
;;       ("v" meghanada-local-variable)

;;       ("t" meghanada-run-task)
;;       ("j" meghanada-run-junit-test-case)
;;       ("J" meghanada-run-junit-class)
;;       ("r" meghanada-run-junit-recent)

;;       ("q" nil)))

;;   (with-eval-after-load 'pophint-autoloads
;;     (pophint-tags:advice-command meghanada-jump-declaration)))


;; エラーになったので一旦コメントアウト
;; (bundle realgud)
;; (use-package realgud
;;   :after (meghanada))


(bundle lsp-java)

(use-package java-mode
  :defer t
  :config
  (when (functionp 'lsp)
    (add-hook 'java-mode-hook 'lsp-deferred t)))


(bundle scala-mode)
(use-package scala-mode
  :defer t
  :init
  (with-eval-after-load 'mmask
    (mmask-regist-extension-with-icase 'scala-mode "scala"))

  :config
  (~add-setup-hook 'scala-mode
    (local-set-key [f1] nil)
    (local-set-key (kbd ".") 'self-insert-with-ac-trigger-command))

  (~add-setup-hook-after-load 'mmask 'scala-mode
    (setq moccur-grep-default-mask (rx-to-string `(or ,(mmask-get-regexp-sexp 'java-mode)
                                                      ,(mmask-get-regexp-sexp 'scala-mode))))))


(bundle kotlin-mode)
(use-package kotlin-mode
  :defer t
  :config
  (when (functionp 'lsp)
    (add-hook 'kotlin-mode-hook 'lsp-deferred t)))


(bundle flycheck-kotlin)
(use-package flycheck-kotlin
  :after (kotlin-mode))

