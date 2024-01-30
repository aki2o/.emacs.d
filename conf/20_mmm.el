;; (use-package mmm-mode
;;   :commands (mmm-mode)
;;   :custom ((mmm-global-mode t)
;;            (mmm-submode-decoration-level 0)))

(use-package polymode
  :defer t
  :custom ((polymode-prefix-key (kbd "C-c p")))
  :config
  (with-eval-after-load 'e2wm-transcribe
    (add-to-list 'polymode-before-switch-buffer-hook '~polymode-switch-buffer-to-left t))
  )

(defun ~polymode-switch-buffer-to-left (&rest args)
  (setq e2wm-transcribe:next-left-p t))
