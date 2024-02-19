(global-font-lock-mode t)
;; (setq-default transient-mark-mode t)
(transient-mark-mode t)
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(line-number-mode t)
(column-number-mode t)
(fringe-mode 1) ;; フリンジ(左右の折り返し表示領域)はいらない
(blink-cursor-mode 1)

(setq-default fill-column 120)
(setq truncate-lines t) ;; 通常時、折り返さない
(setq truncate-partial-width-windows t) ;; 縦分割で行は折り返さない
(setq w32-hide-mouse-on-key t) ;; マウスを消す
(setq w32-hide-mouse-timeout 5000)
(setq cursor-in-non-selected-windows nil) ;; アクティブでないバッファではカーソルを出さない
(setq custom--inhibit-theme-enable nil) ;; https://naokton.hatenablog.com/entry/2020/07/20/065700

(bind-key* "<f3>" 'global-hl-line-mode)
(bind-key* "<f11>" 'global-display-fill-column-indicator-mode)
(bind-key* "<f12>" 'global-display-line-numbers-mode)

;; 現在行のハイライト
(use-package hl-line
  :config
  (set-face-background 'hl-line "gray20"))

(defun my:theme-setup ()
  (interactive)
  (load-theme 'deeper-blue t)
  (custom-theme-set-faces
   'deeper-blue
   '(default                ((t (:foreground "gray90"))))
   '(font-lock-comment-face ((t (:foreground "PaleTurquoise3"))))
   ;; '(mode-line-buffer-id    ((t (:foreground "gray80" :bold nil))))
   '(mode-line              ((t (:foreground "powder blue" :background "DeepSkyBlue4"))))
   '(mode-line-inactive     ((t (:foreground "gray50" :background "gray15"))))
   '(cursor                 ((t (:background "white")))))

  (set-frame-parameter (selected-frame) 'alpha '(85 . 60)) ;; 透明度
  (set-frame-parameter (selected-frame) 'fullscreen 'maximized)
  (set-frame-parameter (selected-frame) 'cursor-type 'hbar)
  (set-frame-parameter (selected-frame) 'cursor-color "white"))

(my:theme-setup)

(defadvice new-frame (after my:theme-setup activate)
  (my:theme-setup))


(use-package doom-modeline
  :custom ((doom-modeline-height 1)
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
   '(doom-modeline-buffer-path ((t (:foreground "light pink" :bold t))))
   '(doom-modeline-buffer-file ((t (:foreground "white"))))
   '(doom-modeline-buffer-major-mode ((t (:foreground "white" :bold t))))))


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
    (kotlin-mode           . "Kt")
    (java-mode             . "Jv")
    (slim-mode             . "Sl")
    (js2-mode              . "Js")
    (coffee-mode           . "Cf")
    (markdown-mode         . "Md")
    (typescript-mode       . "Ts")
    (graphql-mode          . "Gql")))

(defun my:mode-line-setup ()
  (interactive)
  (let ((new-mstr (assoc-default major-mode ~mode-alist))
        (mode-indexes (mapcar 'car ~mode-alist)))
    (when new-mstr
      (setq mode-name new-mstr))
    (setq minor-mode-alist
          (sort (cl-loop for (mode mstr) in minor-mode-alist
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

(add-hook 'after-change-major-mode-hook 'my:mode-line-setup)
