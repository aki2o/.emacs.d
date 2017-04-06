(bundle save-load-path)
(use-package save-load-path
  :config
  (setq save-load-path-file (concat user-emacs-directory ".load-path-saved"))
  (save-load-path-initialize))


(bundle lispxmp)
(use-package lispxmp)


(bundle el-mock)
(bundle ert-expectations)


;; (bundle ert :url "https://raw.github.com/ohler/ert/c619b56c5bc6a866e33787489545b87d79973205/lisp/emacs-lisp/ert.el")
;; (require 'ert)
(bundle emacswiki:el-expectations)
(bundle emacswiki:el-expectations-success-sample)
(bundle emacswiki:el-expectations-failure-sample)
(use-package el-expectations+
  :defer t)


;; (bundle uk-ar/el-spec)
;; (require 'el-spec)


(bundle caskxy)
(use-package caskxy
  :defer t
  :init
  (when (~is-windows)
    (setq caskxy/cask-cli-path (concat user-emacs-directory "elisp/mine/cask-cli.el")))
  (setq caskxy/tester-backend 'el-expectations))


(use-package tenv
  :defer t)


(defun ~find-file-testcase ()
  (interactive)
  (let* ((currpath (buffer-file-name))
         (currfilenm (when (stringp currpath)
                       (file-name-nondirectory currpath)))
         (currpkg (when (and (stringp currfilenm)
                             (string-match "\\.el$" currfilenm)
                             (save-excursion
                               (goto-char (point-min))
                               (re-search-forward "^\\s-*(provide\\s-+'\\([^)]+\\))" nil t)))
                    (match-string-no-properties 1))))
    (if (not currpkg)
        (message "Current buffer is not elisp file.")
      (when (not (file-directory-p (concat user-emacs-directory "test/" currpkg)))
        (mkdir (concat user-emacs-directory "test/" currpkg)))
      (let* ((hist '())
             (testnm (completing-read "Select/Input name of opening testcase: "
                                      (loop for e in (eshell-extended-glob (concat user-emacs-directory "test/" currpkg "/*.el"))
                                            for filenm = (file-name-nondirectory e)
                                            collect (replace-regexp-in-string "\\.el$" "" filenm))
                                      nil
                                      nil
                                      nil
                                      'hist))
             (testnm (replace-regexp-in-string "\\.el$" "" testnm))
             (testfilenm (concat testnm ".el")))
        (switch-to-buffer-other-window
         (find-file-noselect (concat user-emacs-directory "test/" currpkg "/" testfilenm)))
        (when (not (save-excursion
                     (goto-char (point-min))
                     (re-search-forward (concat "^(require '" currpkg ")") nil t)))
          (insert "(require '" currpkg ")\n"
                  "(require 'el-expectations)\n\n"
                  "(expectations\n"
                  "  (desc \"" testnm "\")\n"
                  "  (expect nil\n"
                  "    ))\n"))))))


