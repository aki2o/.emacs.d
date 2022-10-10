(defun ~is-windows ()
  (memq system-type '(windows-nt ms-dos cygwin)))

(defun ~is-mac ()
  (memq system-type '(darwin)))

(defun ~file-exist-in-tree-p (path name &optional directory-p)
  (loop with checker = (if directory-p 'file-directory-p 'file-exists-p)
        with get-parent = (lambda (p)
                            (when (not (string= p "/"))
                              (file-name-directory (directory-file-name p))))
        for nextpath = (funcall get-parent (or nextpath path))
        while (and nextpath
                   (file-directory-p nextpath))
        if (funcall checker (concat (file-name-as-directory nextpath) name))
        return t))

(defun ~tmpdir (&optional name)
  (let ((dir (format "%s/%s"
                     (expand-file-name (or (getenv "TMP")
                                           (getenv "TMPDIR")
                                           (getenv "TEMP")
                                           "/tmp"))
                     name)))
    (when (not (file-directory-p dir))
      (make-directory dir t))
    (directory-file-name dir)))

(defun ~tmpfile (name &optional dirname)
  (format "%s/%s" (~tmpdir dirname) name))

