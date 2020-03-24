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
                          '(mode-line              ((t (:foreground "powder blue" :background "DeepSkyBlue4"))))
                          '(mode-line-inactive     ((t (:foreground "gray50" :background "gray15"))))
                          '(cursor                 ((t (:background "white")))))
  )

(~setup-theme)

(defadvice new-frame (after ~setup-theme activate)
  (~setup-theme))


(bundle all-the-icons)
;; 初期インストール時に (all-the-icons-install-fonts) をやる必要があるっぽい
(use-package all-the-icons
  :custom ((inhibit-compacting-font-caches t)
           (all-the-icons-scale-factor 1.1)
           (all-the-icons-default-adjust -0.1)))


(bundle doom-modeline)
(use-package doom-modeline
  :custom ((doom-modeline-height 10)
           (doom-modeline-bar-width 3)
           (doom-modeline-project-detection 'projectile)
           (doom-modeline-buffer-file-name-style 'relative-to-project)
           (doom-modeline-vcs-max-length 16)
           (doom-modeline-buffer-encoding t)
           (doom-modeline-persp-name nil))
  :init
  (doom-modeline-mode 1)
  :config
  (custom-set-faces
   '(doom-modeline-buffer-path ((t (:foreground "light pink" :bold))))
   '(doom-modeline-buffer-file ((t (:foreground "white"))))
   '(doom-modeline-buffer-major-mode ((t (:foreground "white" :bold))))))


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
