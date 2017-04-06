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


;; load
(bundle! use-package)
(bundle! bind-key)
(bundle! emacs-jp/init-loader)

(init-loader-load (locate-user-emacs-file "conf"))

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

