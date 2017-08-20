(defun ~setup-theme ()
  ;; light
  ;; (load-theme 'tango t)

  ;; dark
  ;; (load-theme 'wombat t)
  ;; (load-theme 'misterioso t)
  ;; (load-theme 'tango-dark t)

  (load-theme 'deeper-blue t)
  (custom-theme-set-faces 'deeper-blue
                          '(default                ((t (:foreground "gray90"))))
                          '(font-lock-comment-face ((t (:foreground "DarkSeaGreen4"))))
                          '(mode-line-buffer-id    ((t (:foreground "gray80" :bold nil))))
                          '(mode-line              ((t (:foreground "black" :background "DeepSkyBlue4"))))
                          '(mode-line-inactive     ((t (:foreground "gray50" :background "gray15"))))
                          '(cursor                 ((t (:background "white")))))
  )

(~setup-theme)

(defadvice new-frame (after ~setup-theme activate)
  (~setup-theme))


(bundle powerline)
(use-package powerline

  :init

  (make-face '~powerline-mode-line-bkup)
  (copy-face 'mode-line '~powerline-mode-line-bkup)
  
  (defvar ~powerline-special-highlight nil)
  
  (defun ~powerline-theme (&rest args)
    (interactive)
    (when current-prefix-arg
      (copy-face '~powerline-mode-line-bkup 'mode-line))
    (when args
      (apply 'set-face-attribute 'mode-line nil args))
    (setq-default
     mode-line-format
     '("%e"
       (:eval
        (let* ((~powerline-special-highlight t)
               (active (powerline-selected-window-active))
               (mode-line (if active 'mode-line 'mode-line-inactive))
               (face1 (if active 'powerline-active1 'powerline-inactive1))
               (face2 (if active 'powerline-active2 'powerline-inactive2))
               (separator-left (intern (format "powerline-%s-%s"
                                               (powerline-current-separator)
                                               (car powerline-default-separator-dir))))
               (separator-right (intern (format "powerline-%s-%s"
                                                (powerline-current-separator)
                                                (cdr powerline-default-separator-dir))))
               (lhs (list (powerline-raw "%*" mode-line)
                          (powerline-raw mode-line-mule-info mode-line)
                          (powerline-raw "%l" mode-line)
                          (powerline-raw ":" mode-line)
                          (powerline-raw "%c" mode-line)
                          (powerline-buffer-id mode-line 'l)
                          (when (and (boundp 'which-func-mode) which-func-mode)
                            (powerline-raw which-func-format mode-line 'l))
                          (powerline-raw " " mode-line)
                          (funcall separator-left mode-line face1)
                          ;; (when (and (boundp 'erc-track-minor-mode) erc-track-minor-mode)
                          ;;   (powerline-raw erc-modified-channels-object face1 'l))
                          (powerline-major-mode face1 'l)
                          (powerline-process face1)
                          (powerline-minor-modes face1 'l)
                          (powerline-narrow face1 'l)
                          (powerline-raw " " face1)
                          (funcall separator-left face1 face2)
                          (powerline-vc face2 'r)
                          (when (bound-and-true-p nyan-mode)
                            (powerline-raw (list (nyan-create)) face2 'l))))
               (rhs (list (powerline-raw global-mode-string face2 'r)
                          (funcall separator-right face2 face1)
                          ;; (unless window-system
                          ;;   (powerline-raw (char-to-string #xe0a1) face1 'l))
                          (powerline-raw "%p" face1 'l)
                          (powerline-raw "/" face1)
                          (powerline-buffer-size face1)
                          ;; (when powerline-display-hud
                          ;;   (powerline-hud face2 face1))
                          )))
          (concat (powerline-render lhs)
                  (powerline-fill face2 (powerline-width rhs))
                  (powerline-render rhs)))))))

  (defun ~powerline-reset-current-separator ()
    (pl/memoize (pl/arrow left))
    (pl/memoize (pl/arrow right)))
  
  (defadvice ~powerline-theme (after ~powerline-update activate)
    (when (or current-prefix-arg
              (ad-get-arg 0))
      (~powerline-reset-current-separator)))

  (defadvice force-mode-line-update (after ~powerline-update activate)
    (~powerline-reset-current-separator))
  
  (~powerline-theme)
  
  :config
  
  (custom-set-faces
   '(powerline-active1   ((t (:background "DeepSkyBlue3" :inherit mode-line))))
   '(powerline-active2   ((t (:background "SkyBlue3" :inherit mode-line))))
   '(powerline-inactive1 ((t (:background "gray30" :inherit mode-line-inactive))))
   '(powerline-inactive2 ((t (:background "gray45" :inherit mode-line-inactive)))))

  (defface ~powerline-vc-revision
    '((t (:foreground "orange")))
    "")

  (defadvice vc-working-revision (after ~powerline-highlight activate)
    (when ~powerline-special-highlight
      (setq ad-return-value
            (propertize ad-return-value 'face '~powerline-vc-revision))))
  
  )


(defface ~minor-mode-special-info
  '((t (:foreground "white")))
  "")

(defvar ~mode-alist
  `(;; Minor modes
    (projectile-mode . (lambda ()
                         (format " PJ:%s"
                                 (propertize
                                  (truncate-string-to-width
                                   (projectile-project-name) 15 nil nil t)
                                  'face '~minor-mode-special-info))))
    (e2wm:pst-minor-mode . (lambda ()
                             (format " WM:%s"
                                     (if (e2wm:managed-p)
                                         (propertize
                                          (format "%s" (e2wm:$pst-name (e2wm:pst-get-instance)))
                                          'face '~minor-mode-special-info)
                                       "none"))))
    ;; Major modes
    (fundamental-mode      . "Fund")
    (dired-mode            . "Dir")
    (lisp-interaction-mode . "Li")
    (emacs-lisp-mode       . "Li")
    (ruby-mode             . "Rb")
    (python-mode           . "Py")
    (perl-mode             . "Pl")
    (cperl-mode            . "Pl")
    (slim-mode             . "Sl")
    (js2-mode              . "Js")
    (coffee-mode           . "Cf")
    (markdown-mode         . "Md")))

(defun ~setup-mode-line ()
  (interactive)
  (let ((new-mstr (assoc-default major-mode ~mode-alist))
        (mode-indexes (mapcar 'car ~mode-alist)))
    (when new-mstr
      (setq mode-name new-mstr))
    (setq minor-mode-alist
          (sort (loop for (mode mstr) in minor-mode-alist
                      for new-mstr = (assoc-default mode ~mode-alist)
                      for new-mstr = (cond ((functionp new-mstr)
                                            (ignore-errors (funcall new-mstr)))
                                           (t
                                            new-mstr))
                      if new-mstr
                      collect `(,mode ,new-mstr))
                (lambda (a b)
                  (> (length (memq (car a) mode-indexes))
                     (length (memq (car b) mode-indexes))))))))

(add-hook 'after-change-major-mode-hook '~setup-mode-line)

