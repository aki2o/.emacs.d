
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

(when (file-exists-p (concat user-emacs-directory "initf.el"))
  (load (concat user-emacs-directory "initf.el")))


;; mine
(when (file-directory-p (concat user-emacs-directory "elisp/mine"))
  (add-to-list 'load-path (concat user-emacs-directory "elisp/mine"))
  (cd (concat user-emacs-directory "elisp/mine"))
  (when (fboundp 'normal-top-level-add-subdirs-to-load-path)
    (normal-top-level-add-subdirs-to-load-path)))


;; el-get
(setq-default el-get-dir (concat user-emacs-directory "elisp/el-get/" emacs-version))
(setq-default package-user-dir (concat user-emacs-directory "elisp/elpa/" emacs-version))
(setq-default el-get-emacswiki-base-url "http://raw.github.com/emacsmirror/emacswiki.org/master/")
(setq-default el-get-notify-type 'message)

(when (and (~is-windows)
           (file-exists-p (concat exec-directory "install-info")))
  (setq-default el-get-install-info (concat exec-directory "install-info")))

(add-to-list 'load-path (expand-file-name "bundle" el-get-dir))

(unless (require 'bundle nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "http://raw.github.com/tarao/bundle-el/master/bundle-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path (locate-user-emacs-file "recipes"))

(bundle! tarao/el-get-lock)
(el-get-lock)
(el-get-lock-unlock 'el-get)


;; load
(bundle! use-package)
(bundle! bind-key)
(bundle! emacs-jp/init-loader)
(init-loader-load (locate-user-emacs-file "conf"))

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(git-gutter:diff-option "-w")
 '(git-gutter:hide-gutter t)
 '(git-gutter:lighter " GG")
 '(mozc-leim-title "[あ]")
 '(package-selected-packages
   (quote
    (dash-functional git-commit ivy org oauth2 gitignore-mode github-browse-file csv-mode company)))
 '(rspec-use-rake-when-possible nil)
 '(terraform-indent-level 2))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(anything-ff-directory ((t (:inherit dired-directory))))
 '(anything-ff-file ((t (:inherit default))))
 '(company-preview-common ((t (:foreground "lightgrey" :background nil :underline t))))
 '(company-scrollbar-bg ((t (:background "gray40"))))
 '(company-scrollbar-fg ((t (:background "orange"))))
 '(company-tooltip ((t (:foreground "black" :background "lightgrey"))))
 '(company-tooltip-common ((t (:foreground "black" :background "lightgrey"))))
 '(company-tooltip-common-selection ((t (:foreground "white" :background "steelblue"))))
 '(company-tooltip-selection ((t (:foreground "black" :background "steelblue"))))
 '(e2wm:face-history-list-normal ((t :foreground "ivory")))
 '(e2wm:face-item ((t :height 0.8 :inherit variable-pitch :foreground "DarkSlateBlue")))
 '(e2wm:face-subtitle ((((class color) (background light)) (:foreground "Gray10" :height 0.8 :inherit variable-pitch)) (((class color) (background dark)) (:foreground "Gray90" :height 0.8 :inherit variable-pitch)) (t :height 0.8 :inherit variable-pitch)))
 '(guide-key-tip/pos-tip-face ((t (:foreground "white" :background "black"))))
 '(ivy-current-match ((t (:background "ForestGreen" :distant-foreground "black"))))
 '(pophint:match-face ((t (:background "dark slate gray" :foreground "white"))))
 '(pophint:pos-tip-face ((t (:background "black" :foreground "white"))))
 '(pophint:tip-face ((t (:background "HotPink4" :foreground "white" :bold t))))
 '(powerline-active1 ((t (:background "DeepSkyBlue3" :inherit mode-line))))
 '(powerline-active2 ((t (:background "SkyBlue3" :inherit mode-line))))
 '(powerline-inactive1 ((t (:background "gray30" :inherit mode-line-inactive))))
 '(powerline-inactive2 ((t (:background "gray45" :inherit mode-line-inactive)))))
