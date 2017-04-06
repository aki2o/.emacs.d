(bundle emacswiki:auto-async-byte-compile)
(use-package auto-async-byte-compile
  :commands (enable-auto-async-byte-compile-mode)
  :init
  (setq auto-async-byte-compile-exclude-files-regexp
        (rx-to-string `(and (or "/.emacs.d/init.el"
                                "/.emacs.d/initf.el"
                                "/.emacs.d/conf/"
                                "/.emacs.d/junk/"
                                "/.emacs.d/test/"))))
  (add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode t))

