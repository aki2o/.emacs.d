(use-package embark
  :after (vertico)
  :init
  (add-hook 'find-file-hook '~embark-setup t)
  (add-hook 'minibuffer-setup-hook '~embark-setup t)
  :config
  (with-eval-after-load 'which-key
    (setq embark-indicators '(~embark-which-key-indicator)))

  (with-eval-after-load 'vertico
    (define-key minibuffer-local-map (kbd "<tab>") 'abort-recursive-edit)
    (define-key vertico-map (kbd "<tab>") '~embark-act-with-completing-read))
  )

(defun ~embark-setup ()
  (setq ~dwim-at-point-function 'embark-act))

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

(defun ~embark-act-with-completing-read (&optional arg)
  (interactive "P")
  (let* ((embark-prompter 'embark-completing-read-prompter)
         (act (propertize "Act" 'face 'highlight))
         (embark-indicators '()))
    (embark-act arg)))


(use-package embark-consult
  :after (embark consult))
