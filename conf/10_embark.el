(bundle embark)
(use-package embark
  :config
  (add-hook 'find-file-hook '~embark-setup t)
  (add-hook 'minibuffer-setup-hook '~embark-setup t)

  (with-eval-after-load 'which-key
    (setq embark-indicators '(~embark-which-key-indicator))
    (advice-add 'embark-completing-read-prompter :after '(lambda () (funcall which-key-custom-hide-popup-function))))
  )

(defun ~embark-setup ()
  (add-to-list '~action-at-point-functions 'embark-act))

(defun ~embark-which-key-indicator ()
  (lambda (&optional keymap targets prefix)
    (if (null keymap)
        (which-key--hide-popup-ignore-command)
      (which-key--show-keymap
       (if (eq (plist-get (car targets) :type) 'embark-become)
           "Become"
         (format "Act on %s '%s'%s"
                 (plist-get (car targets) :type)
                 (embark--truncate-target (plist-get (car targets) :target))
                 (if (cdr targets) "…" "")))
       (if prefix
           (pcase (lookup-key keymap prefix 'accept-default)
             ((and (pred keymapp) km) km)
             (_ (key-binding prefix 'accept-default)))
         keymap)
       nil nil t (lambda (binding)
                   (not (string-suffix-p "-argument" (cdr binding))))))))


(bundle embark-consult :type github :pkgname "oantolin/embark")
(use-package embark-consult
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

