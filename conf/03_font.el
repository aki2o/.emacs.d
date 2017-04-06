(cond ((~is-windows)
       (add-to-list 'default-frame-alist '(font . "Osaka－等幅-12"))
       (when window-system
         (set-default-font "Osaka－等幅-12")
         (set-fontset-font (frame-parameter nil 'font)
                           'japanese-jisx0208
                           '("Osaka-UI" . "unicode-bmp"))))
      ((~is-mac)
       (add-to-list 'default-frame-alist '(font . "Osaka－等幅-13"))
       (when window-system
         (set-default-font "Osaka－等幅-13")
         (set-fontset-font (frame-parameter nil 'font)
                           'japanese-jisx0208
                           '("Osaka-UI" . "unicode-bmp"))))
      (t
       ;; (add-to-list 'default-frame-alist '(font . "ricty-12"))
       (add-to-list 'default-frame-alist '(font . "Osaka－等幅-12"))
       (when window-system
         (set-default-font "Osaka－等幅-12")
         (set-fontset-font (frame-parameter nil 'font)
                           'japanese-jisx0208
                           '("Osaka-UI" . "unicode-bmp"))))
      )

