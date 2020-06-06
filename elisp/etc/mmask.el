(eval-when-compile (require 'cl))

(defvar mmask--extension-hash (make-hash-table :test 'eq))
(defvar mmask--path-regexp-hash (make-hash-table :test 'eq))
(defvar mmask--update-auto-mode-alist-p nil)
(defvar mmask--default-auto-mode-alist nil)

(defun mmask-regist-extension (modesym &rest values)
  (let* ((regists (gethash modesym mmask--extension-hash)))
    (loop for v in values
          if (and (stringp v)
                  (not (string= v "")))
          do (add-to-list 'regists v))
    (puthash modesym regists mmask--extension-hash)
    (mmask--update-auto-mode-alist modesym)))

(defun mmask-regist-extension-with-icase (modesym &rest values)
  (let* ((ivalues (mmask--get-ignore-case-list values)))
    (apply 'mmask-regist-extension modesym ivalues)))

(defun mmask-regist-name (modesym &rest values)
  (let* ((regists (gethash modesym mmask--path-regexp-hash))
         (values (loop for v in values
                       if (and (stringp v)
                               (not (string= v "")))
                       collect v)))
    (add-to-list 'regists `(and (or bos "/") (or ,@values) eos))
    (puthash modesym regists mmask--path-regexp-hash)
    (mmask--update-auto-mode-alist modesym)))

(defun mmask-regist-path (modesym &rest values)
  (let* ((regists (gethash modesym mmask--path-regexp-hash))
         (values (loop for v in values
                       if (and (stringp v)
                               (not (string= v "")))
                       collect v)))
    (add-to-list 'regists `(and bos (or ,@values) eos))
    (puthash modesym regists mmask--path-regexp-hash)
    (mmask--update-auto-mode-alist modesym)))

(defun mmask-regist-name-regexp (modesym &rest values)
  (let* ((regists (gethash modesym mmask--path-regexp-hash)))
    (loop for v in values
          if (and (stringp v)
                  (not (string= v "")))
          do (add-to-list 'regists `(and (or bos "/") (regexp ,v) eos)))
    (puthash modesym regists mmask--path-regexp-hash)
    (mmask--update-auto-mode-alist modesym)))

(defun mmask-regist-path-regexp (modesym &rest values)
  (let* ((regists (gethash modesym mmask--path-regexp-hash)))
    (loop for v in values
          if (and (stringp v)
                  (not (string= v "")))
          do (add-to-list 'regists `(and bos (regexp ,v) eos)))
    (puthash modesym regists mmask--path-regexp-hash)
    (mmask--update-auto-mode-alist modesym)))

(defun mmask-get-regexp-string (modesym)
  (cond ((gethash modesym mmask--extension-hash)
         (rx-to-string `(or (and "."
                                 (or ,@(gethash modesym mmask--extension-hash))
                                 eos)
                            ,@(gethash modesym mmask--path-regexp-hash))))
        ((gethash modesym mmask--path-regexp-hash)
         (rx-to-string `(or ,@(gethash modesym mmask--path-regexp-hash))))
        (t
         "\\`\\0\\'")))

(defun mmask-get-regexp-sexp (modesym)
  (cond ((gethash modesym mmask--extension-hash)
         `(or (and "."
                   (or ,@(gethash modesym mmask--extension-hash))
                   eos)
              ,@(gethash modesym mmask--path-regexp-hash)))
        ((gethash modesym mmask--path-regexp-hash)
         `(or ,@(gethash modesym mmask--path-regexp-hash)))
        (t
         nil)))

(defun mmask-get-mode-matched (filename_or_path)
  (let* ((case-fold-search nil))
    (or (loop for mode being the hash-keys in mmask--extension-hash
              for re = (mmask-get-regexp-string mode)
              if (string-match re filename_or_path)
              return mode)
        (loop for mode being the hash-keys in mmask--path-regexp-hash
              for re = (mmask-get-regexp-string mode)
              if (string-match re filename_or_path)
              return mode))))

(defun mmask-init ()
  (setq mmask--extension-hash (make-hash-table :test 'eq))
  (setq mmask--path-regexp-hash (make-hash-table :test 'eq))
  (setq mmask--update-auto-mode-alist-p nil)
  (mmask--regist-default)
  (mmask--update-auto-mode-alist-all)
  (setq mmask--update-auto-mode-alist-p t))


(defun mmask--get-ignore-case-list (values)
  (loop for v in values
        if (and (stringp v)
                (not (string= v "")))
        append (mmask--get-ignore-case-strings v)))

(defun mmask--get-ignore-case-strings (s)
  ;; (let* ((c (or (when (> (length s) 0)
  ;;                 (substring s 0 1))
  ;;               ""))
  ;;        (remained (or (when (> (length s) 1)
  ;;                        (substring s 1))
  ;;                      ""))
  ;;        (strings (or (when (not (string= remained ""))
  ;;                       (mmask--get-ignore-case-strings remained))
  ;;                     (list ""))))
  ;;   (loop for c in (or (when (string-match "^[a-zA-Z]$" c)
  ;;                        (list (downcase c) (upcase c)))
  ;;                      (list c))
  ;;         append (loop for s in strings
  ;;                      collect (concat c s)))))
  (list (downcase s) (upcase s)))

(defun mmask--update-auto-mode-alist (modesym)
  (when mmask--update-auto-mode-alist-p
    (let* ((re (mmask-get-regexp-string modesym))
           (newal (cond ((rassq modesym mmask--default-auto-mode-alist)
                         auto-mode-alist)
                        (t
                         (loop for e in auto-mode-alist
                               for m = (cdr e)
                               if (not (eq m modesym))
                               collect e)))))
      (add-to-list 'newal (cons re modesym))
      (setq auto-mode-alist newal))))

(defun mmask--update-auto-mode-alist-all ()
  (let* ((newal (loop for e in auto-mode-alist
                      for m = (cdr e)
                      if (not (or (gethash m mmask--extension-hash)
                                  (gethash m mmask--path-regexp-hash)))
                      collect e)))
    (setq mmask--default-auto-mode-alist newal)
    (loop for m being the hash-keys in mmask--extension-hash
          if (not (rassq m newal))
          do (add-to-list 'newal (cons (mmask-get-regexp-string m) m)))
    (loop for m being the hash-keys in mmask--path-regexp-hash
          if (not (rassq m newal))
          do (add-to-list 'newal (cons (mmask-get-regexp-string m) m)))
    (setq auto-mode-alist newal)))

(defun mmask--regist-default ()
  (mmask-regist-extension-with-icase 'ada-mode "ada" "adb" "ads")
  (mmask-regist-extension-with-icase 'antlr-mode "g")
  (mmask-regist-extension-with-icase 'archive-mode
                                     "svgz" "arc" "zip" "lzh" "lha" "zoo" "jar" "ear" "war" "xpi" "rar"
                                     "sxd" "sxm" "sxi" "sxc" "sxw" "odf" "odg" "odp" "ods" "odt" "oxt"
                                     "deb" "opk" "ipk")
  (mmask-regist-extension-with-icase 'asm-mode "s" "asm")
  (mmask-regist-extension-with-icase 'awk-mode "awk")
  (mmask-regist-extension-with-icase 'bibtex-mode "bib")
  (mmask-regist-extension-with-icase 'bibtex-style-mode "bst")
  (mmask-regist-extension-with-icase 'c-mode "xbm" "xpm" "i" "lex" "y" "yacc" "c" "h")
  (mmask-regist-extension-with-icase 'c++-mode "ii" "cc" "hh" "cpp" "cxx" "c++" "hpp" "hxx" "h++")
  (mmask-regist-extension-with-icase 'compilation-mode "gcov")
  (mmask-regist-extension-with-icase 'css-mode "css")
  (mmask-regist-extension-with-icase 'dcl-mode "com")
  (mmask-regist-extension-with-icase 'delphi-mode "dpr")
  (mmask-regist-extension-with-icase 'diff-mode "dif" "diff" "diffs" "pat" "patch" "rej")
  (mmask-regist-extension-with-icase 'dns-mode "soa" "zone")
  (mmask-regist-extension-with-icase 'doc-view-mode "pdf" "dvi")
  (mmask-regist-extension-with-icase 'doctex-mode "dtx")
  (mmask-regist-extension-with-icase 'dsssl-mode "dsl" "dsssl")
  (mmask-regist-extension-with-icase 'ebrowse-tree-mode "ebrowse")
  (mmask-regist-extension-with-icase 'f90-mode "f90" "f95")
  (mmask-regist-extension-with-icase 'fortran-mode "f" "for")
  (mmask-regist-extension-with-icase 'html-mode "htm" "html" "shtm" "shtml" "xhtml")
  (mmask-regist-extension-with-icase 'icon-mode "icn")
  (mmask-regist-extension-with-icase 'idl-mode "idl")
  (mmask-regist-extension-with-icase 'idlwave-mode "pro")
  (mmask-regist-extension-with-icase 'image-mode "pbm" "ppm" "pgm" "pnm" "tif" "tiff" "gif" "png" "jpg" "jpeg")
  (mmask-regist-extension-with-icase 'java-mode "java")
  (mmask-regist-extension-with-icase 'js-mode "js" "jse" "json")
  (mmask-regist-extension-with-icase 'latex-mode "ltx" "sty" "cls" "clo" "bbl")
  (mmask-regist-extension-with-icase 'lisp-mode "l" "lsp" "lisp" "ml" "asd")
  (mmask-regist-extension-with-icase 'm4-mode "m4" "mc")
  (mmask-regist-extension-with-icase 'makefile-automake-mode "am")
  (mmask-regist-extension-with-icase 'makefile-gmake-mode "mk")
  (mmask-regist-extension-with-icase 'makefile-makepp-mode "makepp")
  (mmask-regist-extension-with-icase 'metafont-mode "mf")
  (mmask-regist-extension-with-icase 'metapost-mode "mp")
  (mmask-regist-extension-with-icase 'mixal-mode "mixal")
  (mmask-regist-extension-with-icase 'nroff-mode "mm" "me" "ms" "man" "1" "2" "3" "4" "5" "6" "7" "8" "9")
  (mmask-regist-extension-with-icase 'objc-mode "m")
  (mmask-regist-extension-with-icase 'org-mode "org")
  (mmask-regist-extension-with-icase 'pascal-mode "p" "pas")
  (mmask-regist-extension-with-icase 'perl-mode "pl" "pm" "perl" "pod" "t")
  (mmask-regist-extension-with-icase 'prolog-mode "prolog")
  (mmask-regist-extension-with-icase 'ps-mode "ps" "eps")
  (mmask-regist-extension-with-icase 'python-mode "py")
  (mmask-regist-extension-with-icase 'rst-mode "rst" "rest")
  (mmask-regist-extension-with-icase 'ruby-mode "rb")
  (mmask-regist-extension-with-icase 'scheme-mode "scm" "stk" "ss" "sch" "oak")
  (mmask-regist-extension-with-icase 'scribe-mode "mss")
  (mmask-regist-extension-with-icase 'ses-mode "ses")
  (mmask-regist-extension-with-icase 'sgml-mode "sgm" "sgml" "dtd" "docbook")
  (mmask-regist-extension-with-icase 'sh-mode "sh" "bash" "csh" "ksh" "zsh" "shar" "spec" "mspec")
  (mmask-regist-extension-with-icase 'sieve-mode "sv" "siv" "sieve")
  (mmask-regist-extension-with-icase 'simula-mode "sim")
  (mmask-regist-extension-with-icase 'snmp-mode "asn" "mib" "smi")
  (mmask-regist-extension-with-icase 'snmpv2-mode "as2" "mi2" "sm2")
  (mmask-regist-extension-with-icase 'sql-mode "sql")
  (mmask-regist-extension-with-icase 'srecode-template-mode "srt")
  (mmask-regist-extension-with-icase 'tar-mode "tar" "tgz" "tbz" "tbz2")
  (mmask-regist-extension-with-icase 'tcl-mode "tcl" "itcl" "exp" "itk")
  (mmask-regist-extension-with-icase 'tex-mode "tex" "ins")
  (mmask-regist-extension-with-icase 'texinfo-mode "texinfo" "txi" "texi")
  (mmask-regist-extension-with-icase 'text-mode "txt" "text" "article" "letter")
  (mmask-regist-extension-with-icase 'vera-mode "vr" "vrh" "vri")
  (mmask-regist-extension-with-icase 'verilog-mode "v" "dv" "sv" "vh" "dvh" "svh")
  (mmask-regist-extension-with-icase 'vhdl-mode "vhd" "vhdl")
  (mmask-regist-extension-with-icase 'xml-mode "svg" "xml" "xsl" "dbk")
  (mmask-regist-extension-with-icase 'zone-mode "zone")

  (mmask-regist-name 'sh-mode
                     ".profile" ".bash_profile" ".zprofile"
                     ".login" ".bash_login" ".zlogin"
                     ".logout" ".bash_logout" ".zlogout"
                     ".shrc" ".bashrc" ".cshrc" ".tcshrc" ".kshrc" ".zshrc" ".esrc"
                     ".kshenv" ".zshenv" ".xinitrc" ".startxrc" ".xsession")

  (mmask-regist-name 'autoconf-mode "configure.ac" "Configure.ac" "configure.in" "Configure.in")
  (mmask-regist-name 'ebrowse-tree-mode "BROWSE")
  (mmask-regist-name 'makefile-gmake-mode "makefile" "Makefile")
  (mmask-regist-name 'makefile-imake-mode "imakefile" "Imakefile")
  (mmask-regist-name 'makefile-makepp-mode "makeppfile" "Makeppfile" "makeppfile.mk" "Makeppfile.mk")
  (mmask-regist-name 'ruby-mode "Rakefile")

  (mmask-regist-name-regexp 'change-log-mode "[cC]hange\\.?[lL]og?" "[cC]hange[lL]og[-.][0-9a-z]+" "\\$CHANGE_LOG\\$\\.TXT")
  (mmask-regist-name-regexp 'text-mode "Message[0-9]*")
  (mmask-regist-name-regexp 'scheme-mode ".+\\.scm\\.[0-9]*")
  (mmask-regist-name-regexp 'sh-mode "crontab\\.X*[0-9]+")

  (mmask-regist-path-regexp 'ld-script-mode ".*\\.x[bdsru]?[cn]?" ".*ld\\.?script\\>.*" ".*\\.ld[si]?\\>.*")
  )


(provide 'mmask)
