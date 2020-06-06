(use-package autodisass-java-bytecode
  :defer t)


(use-package google-c-style
  :defer t
  :commands (google-set-c-style))


(use-package meghanada
  :defer t
  :bind (:map meghanada-mode-map
              ("M-RET" . meghanada-local-variable)
              ("C->"   . meghanada-jump-declaration)
              ("C-<"   . meghanada-back-jump)
              ("M-z"   . ~hydra-meghanada/body))
  
  :init
  (setq meghanada-server-remote-debug nil)
  (setq meghanada-javac-xlint "-Xlint:all,-processing")
  
  (defun ~meghanada-setup ()
    (setq indent-tabs-mode nil)
    (setq tab-width 4)
    (setq c-basic-offset 4)
    ;; (google-set-c-style)
    ;; (google-make-newline-indent)
    (meghanada-mode t)
    ;; (add-hook 'before-save-hook 'meghanada-code-beautify-before-save)
    )

  (defhydra ~hydra-meghanada (:hint nil :exit t :idle ~hydra-help-delay)
    "
^Edit^                           ^Tast or Task^
^^^^^^-------------------------------------------------------
_f_: meghanada-compile-file      _m_: meghanada-restart
_c_: meghanada-compile-project   _t_: meghanada-run-task
_o_: meghanada-optimize-import   _j_: meghanada-run-junit-test-case
_s_: meghanada-switch-test-case  _J_: meghanada-run-junit-class
_v_: meghanada-local-variable    _r_: meghanada-run-junit-recent
_q_: exit
"
    ("f" meghanada-compile-file)
    ("m" meghanada-restart)

    ("c" meghanada-compile-project)
    ("o" meghanada-optimize-import)
    ("s" meghanada-switch-test-case)
    ("v" meghanada-local-variable)

    ("t" meghanada-run-task)
    ("j" meghanada-run-junit-test-case)
    ("J" meghanada-run-junit-class)
    ("r" meghanada-run-junit-recent)

    ("q" nil))
  
  :config
  (when (fboundp 'pophint-tags:advice-command)
    (pophint-tags:advice-command meghanada-jump-declaration)))


(use-package realgud
  :after (meghanada))
  

(use-package scala-mode
  :defer t
  :init
  (mmask-regist-extension-with-icase 'scala-mode "scala")
  
  :config
  (defun ~scala-mode-setup ()
    (local-set-key [f1] nil)
    (add-to-list 'ac-sources 'ac-source-yasnippet)
    (local-set-key (kbd ".") 'self-insert-with-ac-trigger-command)
    (setq moccur-grep-default-mask (rx-to-string `(or ,(mmask-get-regexp-sexp 'java-mode)
                                                      ,(mmask-get-regexp-sexp 'scala-mode)))))
  
  (add-hook 'scala-mode-hook '~scala-mode-setup t))


(use-package kotlin-mode
  :defer t)


(use-package flycheck-kotlin
  :after (kotlin-mode))
