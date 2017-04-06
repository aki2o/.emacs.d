(bundle cygwin-mount)
(use-package cygwin-mount
  
  :if (and (~is-windows)
           (executable-find "bash"))

  :config
  
  (cygwin-mount-activate)
  
  ;; /cygdrive/x をx: に置き換える
  (defadvice expand-file-name (before strip-cygdrive (NAME &optional DEFAULT-DIRECTORY) activate)
    "replace /cygdrive/x to x: in path string."
    (if (string-match "^/cygdrive/\\([a-zA-Z]\\)/" NAME)
        (setq NAME (replace-match "\\1:/" nil nil NAME))))

  (use-package mw32script
    :config
    ;; argument-editing の設定
    (mw32script-init))
  
  )

