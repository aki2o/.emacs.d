;; http://blog.daich.org/2015/03/27/el-get-flycheck/
(let ((path-from-shell (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
  (setenv "PATH" path-from-shell)
  (setq exec-path (split-string path-from-shell path-separator)))


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


;; straight.el
(setq straight-use-package-by-default t)
(setq straight-current-profile (format "%d_%d" emacs-major-version emacs-minor-version))
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/master/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(add-to-list 'straight-profiles `(,straight-current-profile . ,(format "%s.el" straight-current-profile)))
(straight-use-package 'use-package)
;; (setq use-package-always-ensure t)


;; load
(use-package bind-key)
(use-package init-loader)
(init-loader-load (locate-user-emacs-file "conf"))

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ag-highlight-search t)
 '(ag-reuse-buffers (quote nil))
 '(ag-reuse-window (quote nil))
 '(git-gutter:diff-option "-w")
 '(git-gutter:hide-gutter t)
 '(git-gutter:lighter " GG")
 '(helm-command-prefix-key "C-x h")
 '(helm-ff-candidate-number-limit 500)
 '(helm-ff-newfile-prompt-p nil)
 '(helm-file-name-case-fold-search t)
 '(helm-split-window-inside-p t)
 '(hydra-hint-display-type (quote posframe))
 '(mozc-leim-title "[あ]")
 '(package-selected-packages
   (quote
    (dap-mode helm-lsp company-box helm-rg rg vue-mode dash-functional git-commit ivy org oauth2 gitignore-mode github-browse-file csv-mode company)))
 '(rspec-use-rake-when-possible nil)
 '(terraform-indent-level 2))
