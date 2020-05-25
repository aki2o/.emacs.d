(require 'el-expectations)
(require 'helm)
(require 'dash)
(require 'save-load-path nil t)

(defcustom el-expectations+/root-directory (concat user-emacs-directory "test")
  ""
  :type 'directory
  :group 'el-expectations)

(defcustom el-expectations+/load-files nil
  ""
  :type '(repeat file)
  :group 'el-expectations)

;;;###autoload
(defun batch-expectations+ ()
  (if (not noninteractive)
      (error "`~batch-expectations' is to be used only with -batch"))
  (destructuring-bind (output-file . lispfiles)
      command-line-args-left
    (dolist (lispfile lispfiles)
      (load (expand-file-name lispfile) nil t))
    (let ((fail-and-errors (expectations-execute)))
      (with-current-buffer expectations-result-buffer
        (write-region (point-min) (point-max) output-file nil 'nodisp))
      (kill-emacs fail-and-errors))))

;;;###autoload
(defun batch-expectations-in-emacs+ (testpkg emacsexe)
  (interactive
   (list
    (completing-read "Select Package: "
                     (loop with dir = (directory-file-name el-expectations+/root-directory)
                           for e in (when (file-directory-p dir)
                                      (directory-files dir t "\\`[a-z]"))
                           for dirpath = (directory-file-name e)
                           for dirnm = (when (file-directory-p dirpath)
                                         (file-name-nondirectory dirpath))
                           if dirnm collect dirnm)
                     nil
                     t)
    (when current-prefix-arg
      (read-file-name "Specify Exe (emacs): " (executable-find "emacs")))))
  (let* ((loadopt (mapconcat (lambda (f) (concat "-l " (shell-quote-argument (expand-file-name f))))
                             el-expectations+/load-files
                             " "))
         (emacsopt (if (and (stringp emacsexe)
                            (not (string= emacsexe "")))
                       (concat "-e " (shell-quote-argument (expand-file-name emacsexe)))
                     ""))
         (rootdir (directory-file-name el-expectations+/root-directory))
         (testdir (expand-file-name (format "%s/%s" rootdir testpkg)))
         (testfiles (directory-files testdir nil "\\.el\\'"))
         (selected (helm :sources `((name . "TestFile")
                                    (candidates . ,(append '("all") testfiles))
                                    (candidate-number-limit . 999)
                                    (action . (lambda (cand)
                                                (or (helm-marked-candidates)
                                                    (list cand)))))))
         (selected (if (-find (lambda (e) (equal e "all")) selected)
                       testfiles
                     selected))
         (testarg (when selected
                    (mapconcat (lambda (f)
                                 (shell-quote-argument (format "%s/%s" testdir f)))
                               selected
                               " "))))
    (if (not testarg)
        (message "Quit")
      (shell-command (format "el-expectations+ %s %s %s" loadopt emacsopt testarg)
                     expectations-result-buffer)
      (with-current-buffer expectations-result-buffer
        (goto-char (point-min))
        (while (re-search-forward "^[0-9].+\\([0-9]\\) failures, \\([0-9]+\\) errors" nil t)
          (put-text-property (match-beginning 0) (match-end 0)
                             'face
                             (if (and (string= "0" (match-string 1))
                                      (string= "0" (match-string 2)))
                                 exps-green-face
                               exps-red-face)))))))

(when (featurep 'save-load-path)
  (add-to-list 'el-expectations+/load-files save-load-path-file t))

(provide 'el-expectations+)
