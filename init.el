(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

(when (file-exists-p (concat user-emacs-directory "initf.el"))
  (load (concat user-emacs-directory "initf.el")))


;; etc
(when (file-directory-p (concat user-emacs-directory "elisp/etc"))
  (add-to-list 'load-path (concat user-emacs-directory "elisp/etc"))
  (cd (concat user-emacs-directory "elisp/etc"))
  (when (fboundp 'normal-top-level-add-subdirs-to-load-path)
    (normal-top-level-add-subdirs-to-load-path)))


(let ((versioned-dir (locate-user-emacs-file (format "elisp/v%s" emacs-version))))
  (setq-default el-get-dir (expand-file-name "el-get" versioned-dir)
                package-user-dir (expand-file-name "elpa" versioned-dir)))


;; bundle (an El-Get wrapper)
(setq-default el-get-emacswiki-base-url
              "http://raw.github.com/emacsmirror/emacswiki.org/master/")
(add-to-list 'load-path (expand-file-name "bundle" el-get-dir))
(unless (require 'bundle nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "http://raw.github.com/tarao/bundle-el/master/bundle-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))
(add-to-list 'el-get-recipe-path (locate-user-emacs-file "recipes"))

(bundle tarao/el-get-lock
  (el-get-lock)
  (el-get-lock-unlock 'el-get 'seq))


;; load
(bundle! use-package)
(bundle! init-loader)
(init-loader-load (locate-user-emacs-file "conf"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(git-gutter:diff-option "-w")
 '(git-gutter:hide-gutter t)
 '(git-gutter:lighter " GG")
 '(lsp-log-io nil nil nil "Customized with use-package lsp-mode")
 '(package-selected-packages
   '(corfu-prescient prescient rbs-mode vue-mode which-key-posframe consult compat bind-key))
 '(rspec-use-rake-flag nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(doom-modeline-buffer-file ((t (:foreground "white"))))
 '(doom-modeline-buffer-major-mode ((t (:foreground "white" :bold t))))
 '(doom-modeline-buffer-path ((t (:foreground "light pink" :bold t))))
 '(e2wm:face-history-list-normal ((t :foreground "ivory")))
 '(e2wm:face-item ((t :height 0.8 :inherit variable-pitch :foreground "DarkSlateBlue")))
 '(e2wm:face-subtitle ((((class color) (background light)) (:foreground "Gray10" :height 0.8 :inherit variable-pitch)) (((class color) (background dark)) (:foreground "Gray90" :height 0.8 :inherit variable-pitch)) (t :height 0.8 :inherit variable-pitch)))
 '(lsp-ui-doc-background ((t :background "gray30")))
 '(pophint:match-face ((t (:background "dark slate gray" :foreground "white"))))
 '(pophint:pos-tip-face ((t (:background "black" :foreground "white"))))
 '(pophint:tip-face ((t (:background "HotPink4" :foreground "white" :bold t))))
 '(which-key-posframe ((t :inherit default :background "gray30")))
 '(which-key-posframe-border ((t (:inherit default :background "gray30")))))
