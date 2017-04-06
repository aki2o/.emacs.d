;; -*- coding: utf-8;  -*-

(eval-when-compile (require 'cl))
(require 'gnus)
(require 'gnus-start)
(require 'gnus-setup)
(require 'gnus-agent)
(require 'gnus-util)
(require 'gnus-art)
(require 'gnus-demon)
(require 'auth-source)
(require 'starttls)
(require 'smtpmail)
(require 'nnimap)
(require 'nnrss)
(require 'nnir)
(require 'bbdb nil t)
(require 'bbdb-hooks nil t)
(require 'gnus-ez-lib)
(require 'gnus-direx nil t)
(require 'e2wm-gnus nil t)
(require 'log4e)
(require 'yaxception)


(defgroup gnus-ez nil
  "Easy/Rich Interface for Gnus."
  :group 'gnus
  :prefix "gnus-ez:")


(defcustom gnus-ez:unchanged-functions nil
  ""
  :type '(repeat function)
  :group 'gnus-ez)

(defcustom gnus-ez:use-authinfo t
  ""
  :type 'boolean
  :group 'gnus-ez)

;; (defcustom gnus-ez:authinfo-file (auth-source-search)
;;   ""
;;   :type 'file
;;   :group 'gnus-ez)

(defcustom gnus-ez:smtp-accounts nil
  ""
  :type 'sexp
  :group 'gnus-ez)

(defcustom gnus-ez:move-to-trash-as-delete t
  ""
  :type 'boolean
  :group 'gnus-ez)

(defcustom gnus-ez:trash-folders '("Trash" "trash" "TRASH" "ごみ箱" "ゴミ箱")
  ""
  :type '(repeat string)
  :group 'gnus-ez)

(defcustom gnus-ez:rss-import-quietly t
  ""
  :type 'boolean
  :group 'gnus-ez)

(defcustom gnus-ez:rss-group-store-file (concat gnus-home-directory "/rss.xml")
  ""
  :type 'file
  :group 'gnus-ez)

(defcustom gnus-ez:new-mail-check-interval '(300 90)
  ""
  :type '(repeat integer)
  :group 'gnus-ez)

(defcustom gnus-ez:new-rss-check-interval '(3650 300)
  ""
  :type '(repeat integer)
  :group 'gnus-ez)


(log4e:deflogger "gnus-ez" "%t [%l] %m" "%H:%M:%S" '((fatal . "fatal")
                                                     (error . "error")
                                                     (warn  . "warn")
                                                     (info  . "info")
                                                     (debug . "debug")
                                                     (trace . "trace")))
(gnus-ez--log-set-level 'trace)


(defmacro gnus-ez::aif (test then &rest else)
  (declare (indent 2))
  `(let ((it ,test)) (if it ,then ,@else)))

(defun gnus-ez:test ()
  (interactive)
  (message "GNUS-EZ!!!"))

(defadvice gnus-ez:test (around ttttt activate)
  (loop for e in '("h" "o")
        do (message "%s" e))
  ad-do-it)

;;;;;;;;;;;;;;;;;;;;;;
;; Behavior of Gnus

(defadvice gnus-group-read-group (before gnus-ez::ad-toggle-display-all disable)
  "Make the all article displayed in default."
  (gnus-ez--trace "do advice gnus-ez::ad-toggle-display-all of gnus-group-read-group")
  (ad-set-arg 0 (not (ad-get-arg 0))))

(defadvice nnimap-request-move-article (before gnus-ez::ad-decode-destination disable)
  "Fix the error when moving to japanese folder."
  (when (ad-get-arg 5)
    (gnus-ez--trace "do advice gnus-ez::ad-decode-destination of nnimap-request-move-article")
    (ad-set-arg 5 (nnimap-decode-gnus-group (ad-get-arg 5)))))

(defun gnus-ez::get-member-group (groups member-list &optional prefix)
  (loop for grp in groups
        for currprefix = (gnus-ez-lib:get-group-prefix grp)
        for realnm = (gnus-group-real-name grp)
        for realnm = (replace-regexp-in-string "\\`.+/\\([^/]+\\)\\'" "\\1" realnm)
        if (and (or (not prefix)
                    (string= currprefix prefix))
                (member realnm member-list))
        return grp))

(defvar gnus-ez::auto-completion-values nil)
(defadvice gnus-completing-read (around gnus-ez::ad-complete-group activate)
  "Do completion automatically by the value that matches `gnus-ez::auto-completion-values'
without `gnus-completing-read-function'."
  (gnus-ez--trace "do advice gnus-ez::ad-complete-group of gnus-completing-read")
  (gnus-ez::aif (and gnus-ez::auto-completion-values
                     (gnus-ez::get-member-group (ad-get-arg 1)
                                                gnus-ez::auto-completion-values
                                                (if (or (not (ad-get-arg 3))
                                                        (string= (ad-get-arg 3) ""))
                                                    (gnus-ez-lib:get-default-prefix)
                                                  (ad-get-arg 3))))
      (setq ad-return-value matched)
    ad-do-it))

(defadvice gnus-completing-read (around gnus-ez::ad-complement-prefix activate)
  "Do `gnus-completing-read-function' with the prefix that is abbreviated in default."
  (gnus-ez--trace "do advice gnus-ez::ad-complement-prefix of gnus-completing-read")
  (lexical-let* ((defprefix (gnus-ez-lib:get-default-prefix))
                 (groups (ad-get-arg 1))
                 (initipt (ad-get-arg 3)))
    ;; Complement of Group
    (ad-set-arg 1 (sort (mapcar 'gnus-ez-lib:get-group-full-name groups) 'string<))
    ;; Complement of initial input
    (when (or (not initipt) (string= initipt ""))
      (setq initipt defprefix))
    (when (string= initipt "nnir:")
      (setq initipt ""))
    (when (and (or (featurep 'anything)
                   (featurep 'helm)))
      (setq initipt (regexp-quote initipt)))
    (ad-set-arg 3 initipt)
    ;; Do body
    ad-do-it
    ;; Complement of return
    (setq ad-return-value
          (replace-regexp-in-string (concat "\\`" (regexp-quote defprefix))
                                    ""
                                    ad-return-value))))

(defvar gnus-ez::read-string-value nil)
(defadvice read-string (around gnus-ez::ad-be-quiet disable)
  ""
  (if (not gnus-ez::read-string-value)
      ad-do-it
    (gnus-ez--trace "do advice gnus-ez::ad-be-quiet of read-string")
    (setq ad-return-value (or (when (stringp gnus-ez::read-string-value)
                                gnus-ez::read-string-value)
                              (ad-get-arg 1)
                              (ad-get-arg 3)
                              ""))))

(defvar gnus-ez::read-from-minibuffer-value nil)
(defadvice read-from-minibuffer (around gnus-ez::ad-be-quiet disable)
  ""
  (if (not gnus-ez::read-from-minibuffer-value)
      ad-do-it
    (gnus-ez--trace "do advice gnus-ez::ad-be-quiet of read-from-minibuffer")
    (setq ad-return-value (or (when (stringp gnus-ez::read-from-minibuffer-value)
                                gnus-ez::read-from-minibuffer-value)
                              (ad-get-arg 1)
                              (ad-get-arg 5)
                              ""))))

(defvar gnus-ez::y-or-n-p-answer nil)
(defadvice y-or-n-p (around gnus-ez::ad-be-quiet disable)
  ""
  (if (not gnus-ez::y-or-n-p-answer)
      ad-do-it
    (gnus-ez--trace "do advice gnus-ez::ad-be-quiet of y-or-n-p")
    (setq ad-return-value (eq gnus-ez::y-or-n-p-answer 'yes))))

(defadvice nnrss-normalize-date (around gnus-ez::ad-fix-date activate)
  ""
  (gnus-ez--trace "do advice gnus-ez::ad-fix-date of nnrss-normalize-date")
  (lexical-let ((dvalue (ad-get-arg 0)))
    (yaxception:$
      (yaxception:try
        ad-do-it)
      (yaxception:catch 'error e
        (gnus-ez--info "failed nnrss-normalize-date by [%s] of [%s] : %s"
                       dvalue subject (yaxception:get-text e))
        (setq ad-return-value (message-make-date))))))

(defadvice nnrss-check-group (before gnus-ez::ad-rec-group activate)
  ""
  (gnus-ez--trace "do advice gnus-ez::ad-rec-group of nnrss-check-group : %s" (ad-get-arg 0)))

(defun gnus-ez:setup-advice ()
  (loop for e in '((gnus-group-read-group before gnus-ez::ad-toggle-display-all)
                   (nnimap-request-move-article before gnus-ez::ad-decode-destination)
                   ;; (gnus-completing-read around gnus-ez::ad-complete-group))
                   )
        for func = (pop e)
        for cls = (pop e)
        for adname = (pop e)
        do (gnus-ez::activate-advice func cls adname)))


;;;;;;;;;;;;;
;; Utility

(defun* gnus-ez::show-message (msg &rest args)
  (apply 'message (concat "[GNUS-EZ] " msg) args)
  nil)

(defun* gnus-ez::y-or-n-p (msg &rest args)
  (y-or-n-p (concat "[GNUS-EZ] " (apply 'format msg args))))

(defun gnus-ez::activate-advice (func cls adname)
  (when (not (memq func gnus-ez:unchanged-functions))
    (ad-enable-advice func cls adname)
    (ad-activate func)))

(defun gnus-ez::inactivate-advice (func cls adname)
  (when (not (memq func gnus-ez:unchanged-functions))
    (ad-disable-advice func cls adname)
    (ad-activate func)))

(defun* gnus-ez::do-quietly (func &key
                                    (read-string-value t)
                                    (read-from-minibuffer-value t)
                                    (y-or-n-p-answer 'yes)
                                    args)
  (yaxception:$
    (yaxception:try
      (gnus-ez::activate-advice 'read-string 'around 'gnus-ez::ad-be-quiet)
      (setq gnus-ez::read-string-value read-string-value)
      (gnus-ez::activate-advice 'read-from-minibuffer 'around 'gnus-ez::ad-be-quiet)
      (setq gnus-ez::read-from-minibuffer-value read-from-minibuffer-value)
      (gnus-ez::activate-advice 'y-or-n-p 'around 'gnus-ez::ad-be-quiet)
      (setq gnus-ez::y-or-n-p-answer y-or-n-p-answer)
      (apply func args))
    (yaxception:catch 'error e
      (gnus-ez::show-message (yaxception:get-text e))
      (gnus-ez--error "failed do quietly : %s\n%s"
                        (yaxception:get-text e)
                        (yaxception:get-stack-trace-string e)))
    (yaxception:finally
      (gnus-ez::inactivate-advice 'read-string 'around 'gnus-ez::ad-be-quiet)
      (setq gnus-ez::read-string-value nil)
      (gnus-ez::inactivate-advice 'read-from-minibuffer 'around 'gnus-ez::ad-be-quiet)
      (setq gnus-ez::read-from-minibuffer-value nil)
      (gnus-ez::inactivate-advice 'y-or-n-p 'around 'gnus-ez::ad-be-quiet)
      (setq gnus-ez::y-or-n-p-answer nil))))

(defun gnus-ez::collect-function-suffix (prefix)
  (loop for sym in (apropos-internal (concat "\\`" prefix ".+\\'"))
        for fname = (replace-regexp-in-string (concat "\\`" prefix) "" (symbol-name sym))
        if (functionp sym)
        collect fname))

(defun gnus-ez:scroll-left ()
  (interactive)
  (scroll-left 10 t))

(defun gnus-ez:scroll-right ()
  (interactive)
  (scroll-right 10 t))


;;;;;;;;;;;;;;
;; AuthInfo

;; (defvar gnus-ez::authinfo-edit-buffer " *GNUS-EZ authinfo*")

;; (defun gnus-ez::chk-authinfo-environment ()
;;   (or (executable-find "gpg")
;;       (progn (gnus-ez::show-message "Not found command : gpg")
;;              nil)))

;; (defun gnus-ez:authinfo-edit ()
;;   ""
;;   (interactive)
;;   (yaxception:$
;;     (yaxception:try
;;       (when (gnus-ez::chk-authinfo-environment)
;;         (with-current-buffer (get-buffer-create gnus-ez::authinfo-edit-buffer)
;;           (erase-buffer)
;;           (when (file-exists-p gnus-ez:authinfo-file)
;;             (shell-command (format "gpg --decrypt %s 2>/dev/null"
;;                                    (shell-quote-argument (expand-file-name gnus-ez:authinfo-file)))
;;                            (current-buffer)))
;;           (pop-to-buffer (current-buffer)))))
;;     (yaxception:catch 'error e
;;       (gnus-ez::show-message "Failed authinfo edit : %s" (yaxception:get-text e))
;;       (gnus-ez--error "failed authinfo edit : %s\n%s"
;;                         (yaxception:get-text e)
;;                         (yaxception:get-stack-trace-string e)))))

;; (defun gnus-ez:authinfo-save ()
;;   ""
;;   (interactive)
;;   (yaxception:$
;;     (yaxception:try
;;       (when (and (gnus-ez::chk-authinfo-environment)
;;                  (string= (buffer-name) gnus-ez::authinfo-edit-buffer))
;;         (let ((tmpfile (replace-regexp-in-string "\\.gpg$" "" (expand-file-name gnus-ez:authinfo-file))))
;;           (write-region (point-min) (point-max) tmpfile nil 'visit)
;;           (when (file-exists-p gnus-ez:authinfo-file)
;;             (delete-file gnus-ez:authinfo-file))
;;           (if (file-exists-p gnus-ez:authinfo-file)
;;               (gnus-ez::show-message "Failed initialize %s" gnus-ez:authinfo-file)
;;             (shell-command (format "gpg -c --cipher-algo aes %s" (shell-quote-argument tmpfile)))
;;             (when (file-exists-p tmpfile)
;;               (delete-file tmpfile))
;;             (kill-buffer)
;;             (when (gnus-ez::y-or-n-p "Finished save authinfo. Confirm the saved content?")
;;               (shell-command (format "gpg --decrypt %s 2>/dev/null"
;;                                      (shell-quote-argument (expand-file-name gnus-ez:authinfo-file)))))))))
;;     (yaxception:catch 'error e
;;       (gnus-ez::show-message "Failed authinfo save : %s" (yaxception:get-text e))
;;       (gnus-ez--error "failed authinfo save : %s\n%s"
;;                         (yaxception:get-text e)
;;                         (yaxception:get-stack-trace-string e)))))

(defun gnus-ez:setup-authinfo ()
  (when gnus-ez:use-authinfo
    ;; (setq auth-sources (list gnus-ez:authinfo-file))
    ;; (setq nntp-authinfo-file gnus-ez:authinfo-file)
    ;; (add-to-list 'auth-sources gnus-ez:authinfo-file)
    (setq nntp-authinfo-file (loop for f in auth-sources if (and f (file-exists-p f)) return f))
    ;; (setq nnimap-authinfo-file gnus-ez:authinfo-file)
    ;; (setq smtpmail-auth-credentials gnus-ez:authinfo-file)
    ;; (setq encrypt-file-alist '((,gnus-ez:authinfo-file (gpg "AES"))))
    ))


;;;;;;;;;;;;
;; Format

;; Avoid compiler warnings
(eval-when-compile
  (defvar gnus-tmp-decoded-group)
  (defvar gnus-tmp-group))

(defun gnus-user-format-function-x (dummy)
  "Return the format string as 'SERVER GROUP' by %ux in `gnus-group-line-format'."
  (let ((group (if (boundp 'gnus-tmp-decoded-group)
                   gnus-tmp-decoded-group
                 gnus-tmp-group)))
    (yaxception:$
      (yaxception:try
        (multiple-value-bind (mtd svr) (gnus-ez-lib:get-group-method/server group)
          (let* ((defsvr (gnus-ez-lib:get-default-server))
                 (svrwide (number-to-string (length defsvr))))
            (cond (svr
                   (concat svr " " (gnus-group-real-name group)))
                  ((and mtd (string= mtd "nnrss"))
                   (concat (format (concat "%-" svrwide "s ") "RSS") (gnus-group-real-name group)))
                  ((and mtd (not (string= mtd "")))
                   group)
                  (t
                   (concat defsvr " " group))))))
      (yaxception:catch 'error e
        (gnus-ez--error "failed gnus-user-format-function-x : %s\n%s"
                          (yaxception:get-text e)
                          (yaxception:get-stack-trace-string e))
        (concat group " *FORMAT-ERROR*")))))

(defun gnus-ez:setup-format ()
  (setq gnus-permanently-visible-groups "[^a-z:]")
  (setq gnus-group-line-format "%S%m %ux (%y/%t)\n")
  (setq gnus-group-sort-function 'gnus-group-sort-by-server)
  (setq gnus-summary-line-format "%U%R %&user-date; %[%-20,20f%] %S\n")
  (setq gnus-user-date-format-alist '(((gnus-seconds-today) . "Today %H:%M")
                                      (604800               . "%m/%d %H:%M")
                                      (t                    . " %Y/%m/%d")))
  (setq gnus-show-threads nil)
  (setq gnus-article-sort-functions '(lambda (h1 h2) (gnus-article-sort-by-date h2 h1)))
  )


;;;;;;;;;;
;; Sort

(defun gnus-ez::get-sort-condition (sort-types)
  (let* ((cands (loop for type in sort-types
                      append (list (concat type " [asc]")
                                   (concat type " [desc]"))))
         (selected (when cands (completing-read "" cands nil t)))
         (parts (when selected (split-string selected " ")))
         (type (when parts (pop parts)))
         (order (when parts (pop parts)))
         (reverse (and order (string= order "[desc]"))))
    (list type reverse)))

(defvar gnus-ez::group-sort-types nil)
(defvar gnus-ez::group-sort-prefix "gnus-group-sort-by-")
(defun gnus-ez:setup-group-sort ()
  (setq gnus-ez::group-sort-types
        (gnus-ez::collect-function-suffix gnus-ez::group-sort-prefix))
  (gnus-ez--debug "found group sort types : %s" gnus-ez::group-sort-types))

(defvar gnus-ez::article-sort-types nil)
(defvar gnus-ez::article-sort-prefix "gnus-article-sort-by-")
(defun gnus-ez:setup-article-sort ()
  (setq gnus-ez::article-sort-types
        (gnus-ez::collect-function-suffix gnus-ez::article-sort-prefix))
  (gnus-ez--debug "found article sort types : %s" gnus-ez::article-sort-types))


;;;;;;;;;;;;;;;;;;
;; Address Book

(defun gnus-ez:setup-addrbook ()
  (if (not (featurep 'bbdb))
      (gnus-ez--info "bbdb is not available")
    (gnus-ez--trace "start setup addrbook")
    ;; (setq bbdb/news-auto-create-p t)
    ;; (setq bbdb-always-add-addresses t)
    ;; (setq bbdb-use-pop-up nil)
    ;; (setq bbdb-offer-save nil)
    ;; (bbdb-initialize 'gnus 'message)
    ;; (when (featurep 'bbdb-hooks)
    ;;   (setq bbdb-auto-notes-alist '(("X-Mailer"     ("\(xcite[^>]+> \|\)\(.*\)" mua 2))
    ;;                                 ("X-ML-Name"    (".*$" ML 0))
    ;;                                 ("X-Newsreader" ("\(xcite[^>]+> \|\)\(.*\)" mua 2))
    ;;                                 ("X-Emacs"      (".*" emacs 0))
    ;;                                 ("X-URL"        (".*" WWW 0))
    ;;                                 ("X-Url"        (".*" www 0))
    ;;                                 ("X-URI"        (".*" WWW 0))
    ;;                                 ("X-Uri"        (".*" www 0))
    ;;                                 ("X-Sequence"   ("^[^ \t\n]+" lists 0))
    ;;                                 ("User-Agent"   ("\(xcite[^>]+> \|\)\(.*\)" mua 2))
    ;;                                 ("To"           ("kose@" ml "direct"))
    ;;                                 ("Reply-To"     ("^\([^@]+\)@" ml 1))
    ;;                                 ("reply-to"     ("^\([^@]+\)@" ml 1))
    ;;                                 ("Organization" (".*" Organization 0))))
    ;;   (add-hook 'bbdb-notice-hook 'bbdb-auto-notes-hook))
    ))


;;;;;;;;;;
;; SMTP

(defun gnus-ez:setup-smtp-account ()
  (multiple-value-bind (mtd svr) (gnus-ez-lib:get-group-method/server gnus-newsgroup-name)
    (let* ((defsvr (gnus-ez-lib:get-default-server))
           (key (or svr defsvr))
           (accountinfo (or (assoc-default key gnus-ez:smtp-accounts)
                            (cdr (assq 'default gnus-ez:smtp-accounts)))))
      (if (not accountinfo)
          (gnus-ez::show-message "Not found smtp account of %s or default" key)
        (loop for e in accountinfo
              do (set (car e) (cdr e)))))))


;;;;;;;;;
;; RSS

(defun gnus-ez:rss-import (opml-file)
  ""
  (interactive "fImport file: ")
  (yaxception:$
    (yaxception:try
      (gnus-ez--trace "start rss import : %s" opml-file)
      (if (not (file-exists-p opml-file))
          (gnus-ez::show-message "Not exist file : %s" opml-file)
        (if gnus-ez:rss-import-quietly
            (gnus-ez::do-quietly 'nnrss-opml-import :args (list opml-file))
          (nnrss-opml-import opml-file)))
      (gnus-ez::show-message "Finished rss import")
      (gnus-ez--trace "finished rss import : %s" opml-file))
    (yaxception:catch 'error e
      (gnus-ez::show-message "Failed rss import : %s" (yaxception:get-text e))
      (gnus-ez--error "failed rss import : %s\n%s"
                        (yaxception:get-text e)
                        (yaxception:get-stack-trace-string e)))))

(defun gnus-ez:rss-export (opml-file)
  ""
  (interactive (list (read-string "Export file: " gnus-ez:rss-group-store-file)))
  (yaxception:$
    (yaxception:try
      (gnus-ez--trace "start rss export : %s" opml-file)
      (save-window-excursion
        (nnrss-opml-export)
        (let ((buff (get-buffer "*OPML Export*")))
          (with-current-buffer buff
            (write-region (point-min) (point-max) opml-file))
          (kill-buffer buff)))
      (gnus-ez::show-message "Finished rss export")
      (gnus-ez--trace "finished rss export : %s" opml-file))
    (yaxception:catch 'error e
      (gnus-ez::show-message "Failed rss export : %s" (yaxception:get-text e))
      (gnus-ez--error "failed rss export : %s\n%s"
                        (yaxception:get-text e)
                        (yaxception:get-stack-trace-string e)))))


;;;;;;;;;;;
;; Daemon

(defun gnus-ez::daemon-get-new-news (backends &optional with-default)
  (yaxception:$
    (yaxception:try
      (gnus-ez--trace "start daemon get new news about %s" backends)
      (let ((gnus-newsrc-alist (loop for e in gnus-newsrc-alist
                                     for grp = (gnus-group-decoded-name (nth 0 e))
                                     if (multiple-value-bind (mtd svr) (gnus-ez-lib:get-group-method/server grp)
                                          (or (and mtd (member mtd backends))
                                              (and with-default (not mtd))))
                                     collect (progn (gnus-ez--trace "found target of getting new news : %s" grp)
                                                    e))))
        (gnus-demon-scan-news))
      (gnus-group-list-groups)
      (gnus-ez--trace "finished daemon get new news about %s" backends))
    (yaxception:catch 'error e
      (gnus-ez::show-message "Failed get new news : %s" (yaxception:get-text e))
      (gnus-ez--error "failed daemon get new news about %s : %s\n%s"
                        backends
                        (yaxception:get-text e)
                        (yaxception:get-stack-trace-string e)))))

(defun gnus-ez::set-daemon (func interval-info)
  (let* ((normal-interval (nth 0 interval-info))
         (idle-interval (nth 1 interval-info)))
    (loop for e in gnus-demon-handlers
          for currfunc = (nth 0 e)
          if (eq currfunc func)
          do (setq gnus-demon-handlers (delq e gnus-demon-handlers)))
    (when normal-interval
      (push (list func normal-interval nil) gnus-demon-handlers))
    (when idle-interval
      (push (list func nil idle-interval) gnus-demon-handlers))))

(defun gnus-ez:daemon-get-new-mail ()
  (gnus-ez::daemon-get-new-news '("nnimap" "nnmail" "nnmbox" "nnbabyl" "nnml" "nnmh" "nnmaildir" "nnfolder") t))

(defun gnus-ez:daemon-get-new-rss ()
  (gnus-ez::daemon-get-new-news '("nnrss")))

(defun gnus-ez:setup-daemon ()
  (gnus-ez--trace "start setup daemon")
  (setq gnus-demon-timestep 1)
  (gnus-ez::set-daemon 'gnus-ez:daemon-get-new-mail gnus-ez:new-mail-check-interval)
  (gnus-ez::set-daemon 'gnus-ez:daemon-get-new-rss gnus-ez:new-rss-check-interval)
  (gnus-demon-init))


;;;;;;;;;;;;;;;;;;;;
;; For Group Mode

(defun gnus-ez:group-select-group ()
  ""
  (interactive)
  (yaxception:$
    (yaxception:try
      (gnus-ez::do-quietly 'gnus-group-select-group
                             :read-string-value (number-to-string gnus-large-newsgroup)))
    (yaxception:catch 'error e
      (gnus-ez::show-message "Failed select group : %s" (yaxception:get-text e))
      (gnus-ez--error "failed group select group : %s\n%s"
                        (yaxception:get-text e)
                        (yaxception:get-stack-trace-string e)))))

(defun gnus-ez:group-sort-group ()
  ""
  (interactive)
  (yaxception:$
    (yaxception:try
      (if (not gnus-ez::group-sort-types)
          (gnus-ez::show-message "Not yet execute gnus-ez:setup-group-sort")
        (multiple-value-bind (type reverse) (gnus-ez::get-sort-condition gnus-ez::group-sort-types)
          (let ((func (when type (intern-soft (concat gnus-ez::group-sort-prefix type)))))
            (if (or (not func)
                    (not (functionp func)))
                (gnus-ez::show-message "The selected value is illegal")
              (gnus-group-sort-groups func reverse))))))
    (yaxception:catch 'error e
      (gnus-ez::show-message "Failed sort group : %s" (yaxception:get-text e))
      (gnus-ez--error "failed group sort group : %s\n%s"
                        (yaxception:get-text e)
                        (yaxception:get-stack-trace-string e)))))

(defun gnus-ez:setup-group ()
  (yaxception:$
    (yaxception:try
      (gnus-ez--trace "start setup group")
      (gnus-ez:setup-group-sort)
      (gnus-ez:setup-article-sort)
      (gnus-group-sort-groups gnus-group-sort-function)
      (gnus-ez:setup-daemon))
    (yaxception:catch 'error e
      (gnus-ez::show-message "Failed setup group : %s" (yaxception:get-text e))
      (gnus-ez--error "failed setup group : %s\n%s"
                        (yaxception:get-text e)
                        (yaxception:get-stack-trace-string e)))))


;;;;;;;;;;;;;;;;;;;;;;
;; For Summary Mode

(defun gnus-ez::summary-insert-old-articles ()
  (gnus-ez::do-quietly 'gnus-summary-insert-old-articles
                         :read-string-value (number-to-string gnus-large-newsgroup)))

(defun gnus-ez:summary-move-article (&optional n to-newsgroup select-method)
  (interactive "P")
  (yaxception:$
    (yaxception:try
      ;; (gnus-ez::activate-advice 'gnus-completing-read 'around 'gnus-ez::ad-complement-prefix)
      (gnus-summary-move-article n to-newsgroup select-method))
    (yaxception:catch 'error e
      (gnus-ez::show-message "Failed move article : %s" (yaxception:get-text e))
      (gnus-ez--error "failed summary move article : %s\n%s"
                        (yaxception:get-text e)
                        (yaxception:get-stack-trace-string e)))
    (yaxception:finally
      ;; (gnus-ez::inactivate-advice 'gnus-completing-read 'around 'gnus-ez::ad-complement-prefix))))
      )))

(defun gnus-ez:summary-delete-article (&optional n select-method)
  (interactive "P")
  (yaxception:$
    (yaxception:try
      (if (not gnus-ez:move-to-trash-as-delete)
          (gnus-summary-delete-article n)
        (setq gnus-ez::auto-completion-values gnus-ez:trash-folders)
        (gnus-summary-move-article n nil select-method)))
    (yaxception:catch 'error e
      (gnus-ez::show-message "Failed delete article : %s" (yaxception:get-text e))
      (gnus-ez--error "failed summary delete article : %s\n%s"
                        (yaxception:get-text e)
                        (yaxception:get-stack-trace-string e)))
    (yaxception:finally
      (setq gnus-ez::auto-completion-values nil))))

(defun gnus-ez:summary-toggle-mark-as-processable (n)
  (interactive "P")
  (yaxception:$
    (yaxception:try
      (let ((article (gnus-summary-article-number)))
        (if (and article (memq article gnus-newsgroup-processable))
            (gnus-summary-unmark-as-processable n)
          (gnus-summary-mark-as-processable n))))
    (yaxception:catch 'error e
      (gnus-ez::show-message "Failed toggle mark as processable : %s" (yaxception:get-text e))
      (gnus-ez--error "failed summary toggle mark as processable : %s\n%s"
                        (yaxception:get-text e)
                        (yaxception:get-stack-trace-string e)))))

(defun gnus-ez:summary-next-subject (n)
  (interactive "p")
  (yaxception:$
    (yaxception:try
      (let ((startpt (point)))
        (gnus-summary-next-subject n)
        (when (eq (point) startpt)
          (gnus-ez::summary-insert-old-articles))
        (when (eq (point) startpt)
          (gnus-ez::show-message "The downloaded article seems nothing any more."))))
    (yaxception:catch 'error e
      (gnus-ez::show-message "Failed next subject : %s" (yaxception:get-text e))
      (gnus-ez--error "failed summary next subject : %s\n%s"
                        (yaxception:get-text e)
                        (yaxception:get-stack-trace-string e)))))

(defun gnus-ez:summary-next-unread-subject (n)
  (interactive "p")
  (yaxception:$
    (yaxception:try
      (let ((startpt (point)))
        (gnus-summary-next-unread-subject n)
        (when (eq (point) startpt)
          (gnus-ez::summary-insert-old-articles))
        (when (eq (point) startpt)
          (gnus-ez::show-message "The downloaded article seems nothing any more."))))
    (yaxception:catch 'error e
      (gnus-ez::show-message "Failed next unread subject : %s" (yaxception:get-text e))
      (gnus-ez--error "failed summary next unread subject : %s\n%s"
                        (yaxception:get-text e)
                        (yaxception:get-stack-trace-string e)))))

(defun gnus-ez:summary-sort-article ()
  ""
  (interactive)
  (yaxception:$
    (yaxception:try
      (if (not gnus-ez::article-sort-types)
          (gnus-ez::show-message "Not yet execute gnus-ez:setup-article-sort")
        (multiple-value-bind (type reverse) (gnus-ez::get-sort-condition gnus-ez::article-sort-types)
          (if (not type)
              (gnus-ez::show-message "The selected value is illegal")
            (gnus-summary-sort type reverse)))))
    (yaxception:catch 'error e
      (gnus-ez::show-message "Failed sort article : %s" (yaxception:get-text e))
      (gnus-ez--error "failed summary sort article : %s\n%s"
                        (yaxception:get-text e)
                        (yaxception:get-stack-trace-string e)))))

(defun gnus-ez:setup-summary ()
  (yaxception:$
    (yaxception:try
      (gnus-ez:setup-smtp-account))
    (yaxception:catch 'error e
      (gnus-ez::show-message "Failed setup summary : %s" (yaxception:get-text e))
      (gnus-ez--error "failed setup summary : %s\n%s"
                        (yaxception:get-text e)
                        (yaxception:get-stack-trace-string e)))))


;;;;;;;;;;;;;;;;;
;; Key binding

(defun gnus-ez:setup-keymap ()
  ""
  (define-key gnus-group-mode-map (kbd "j") 'gnus-group-next-group)
  (define-key gnus-group-mode-map (kbd "k") 'gnus-group-prev-group)
  (define-key gnus-group-mode-map (kbd "J") 'gnus-group-next-unread-group)
  (define-key gnus-group-mode-map (kbd "K") 'gnus-group-prev-unread-group)
  (define-key gnus-group-mode-map (kbd "s") 'gnus-ez:group-sort-group)
  (define-key gnus-group-mode-map (kbd "RET") 'gnus-ez:group-select-group)
  
  (define-key gnus-agent-summary-mode-map (kbd "J") nil)
  
  (define-key gnus-summary-mode-map (kbd "j") 'gnus-ez:summary-next-subject)
  (define-key gnus-summary-mode-map (kbd "k") 'gnus-summary-prev-subject)
  (define-key gnus-summary-mode-map (kbd "h") 'gnus-ez:scroll-right)
  (define-key gnus-summary-mode-map (kbd "l") 'gnus-ez:scroll-left)
  (define-key gnus-summary-mode-map (kbd "J") 'gnus-ez:summary-next-unread-subject)
  (define-key gnus-summary-mode-map (kbd "K") 'gnus-summary-prev-unread-subject)
  (define-key gnus-summary-mode-map (kbd "SPC") 'scroll-up)
  (define-key gnus-summary-mode-map (kbd "b") 'scroll-down)
  (define-key gnus-summary-mode-map (kbd "^") 'beginning-of-line)
  (define-key gnus-summary-mode-map (kbd "$") 'end-of-line)
  (define-key gnus-summary-mode-map (kbd "<") 'beginning-of-buffer)
  (define-key gnus-summary-mode-map (kbd ">") 'end-of-buffer)
  (define-key gnus-summary-mode-map (kbd "d") 'gnus-ez:summary-delete-article)
  (define-key gnus-summary-mode-map (kbd "D") 'gnus-ez:summary-move-article)
  (define-key gnus-summary-mode-map (kbd "m") 'gnus-ez:summary-toggle-mark-as-processable)
  (define-key gnus-summary-mode-map (kbd "a") 'gnus-summary-put-mark-as-read-next-unread)
  (define-key gnus-summary-mode-map (kbd "u") 'gnus-summary-clear-mark-forward)
  (define-key gnus-summary-mode-map (kbd "U") 'gnus-summary-clear-mark-backward)
  (define-key gnus-summary-mode-map (kbd "s") 'gnus-ez:summary-sort-article)
  
  )

(defun gnus-ez:default-setup ()
  ""
  (yaxception:$
    (yaxception:try
      (gnus-ez--trace "start default setup")
      ;; For Meadow
      (when (and (not utf7-utf-16-coding-system)
                 (mm-coding-system-p 'utf-16be)
                 (= 2 (length (encode-coding-string "a" 'utf-16be))))
        (setq utf7-utf-16-coding-system 'utf-16be))
      (gnus-ez:setup-keymap)
      (gnus-ez:setup-format)
      (gnus-ez:setup-authinfo)
      (gnus-ez:setup-advice)
      (gnus-ez:setup-addrbook)
      (add-hook 'gnus-started-hook 'gnus-ez:setup-group t)
      (add-hook 'gnus-select-group-hook 'gnus-ez:setup-summary t)
      (gnus-direx:default-setup)
      (e2wm-gnus:default-setup)
      (gnus-ez--trace "finished default setup"))
    (yaxception:catch 'error e
      (gnus-ez::show-message "Failed default setup : %s" (yaxception:get-text e))
      (gnus-ez--error "failed default setup : %s\n%s"
                        (yaxception:get-text e)
                        (yaxception:get-stack-trace-string e)))))


(provide 'gnus-ez)
