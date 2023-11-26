(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

(load (locate-user-emacs-file "initf.el"))

(let ((elisp-dir (locate-user-emacs-file "elisp/")))
  ;; package
  (require 'package)
  (setq-default package-user-dir (concat elisp-dir "elpa"))
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
  (setq package-enable-at-startup nil)

  ;; borg
  (setq borg-drones-directory (concat elisp-dir "borg"))
  (add-to-list 'load-path (expand-file-name "borg" borg-drones-directory))
  (require 'borg-elpa)
  (borg-elpa-initialize)

  ;; etc
  (add-to-list 'load-path (concat elisp-dir "etc"))
  (cd (concat elisp-dir "etc"))
  (normal-top-level-add-subdirs-to-load-path))

;; load
(require 'use-package)
(require 'init-loader)
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
