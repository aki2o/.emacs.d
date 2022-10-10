;; もう不要な気がするから、一旦コメントアウト
;; (bundle auto-async-byte-compile)
;; (use-package auto-async-byte-compile
;;   :defer t
;;   :commands (enable-auto-async-byte-compile-mode)
;;   :hook (emacs-lisp-mode . enable-auto-async-byte-compile-mode)
;;   :init
;;   (setq auto-async-byte-compile-exclude-files-regexp
;;         (rx-to-string `(and (or "/.emacs.d/init.el"
;;                                 "/.emacs.d/initf.el"
;;                                 "/.emacs.d/conf/"
;;                                 "/.emacs.d/junk/"
;;                                 "/.emacs.d/test/")))))
