(eval-when-compile (require 'cl))
(require 'rx)
(require 'regexp-opt)
(require 'em-glob)
(require 'url)
(require 'deferred)
(require 'concurrent)
(require 'auto-complete)
(require 'auto-complete-config)
(require 'log4e)
(require 'yaxception)


(defgroup dtd-completion nil
  "Auto completion for DTD."
  :group 'completion
  :prefix "dtdcmp-")

(defcustom dtdcmp-dtd-cache-directory "~/.dtd.d"
  "Directory of storing downloaded DTD."
  :type 'directory
  :group 'dtd-completion)

(defcustom dtdcmp-default-dtd-url "http://www.w3.org/TR/html4/loose.dtd"
  "If not found DTD URL by defined DOCTYPE in buffer, use this."
  :type 'string
  :group 'dtd-completion)
;; http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd

(defcustom dtdcmp-default-root-tag "html"
  "If not found root tag by define DOCTYPE in buffer, use this."
  :type 'string
  :group 'dtd-completion)

(defcustom dtdcmp-download-timeout 120000
  "Time of waiting downloading DTD (msec)."
  :type 'integer
  :group 'dtd-completion)


(log4e:deflogger "dtdcmp"
                 "%t [%l] %m"
                 "%H:%M:%S"
                 '((fatal . "fatal")
                   (error . "error")
                   (warn  . "warn")
                   (info  . "info")
                   (debug . "debug")
                   (trace . "trace")))
(dtdcmp--log-enable-debugging)

(yaxception:deferror 'dtdcmp-parse-error nil "[DTDCMP] %s : %s" 'desc 'orgmsg)


(defstruct dtdcmp-dtd url state errmsg (text "") (tagh (make-hash-table :test 'equal)) (entityh (make-hash-table :test 'equal)) redirecturl)
(defstruct dtdcmp-entity name value available)
(defstruct dtdcmp-tag name children (attrh (make-hash-table :test 'equal)) validate-re anychild hastext abbr-start abbr-end)
(defstruct dtdcmp-attr name cands required editable default validate-func)


(defvar dtdcmp-regexp-entitynm "[a-zA-Z][a-zA-Z0-9_.-]*")
(defvar dtdcmp-regexp-cache-line (rx-to-string `(and bol (group (+ numeric)) (+ space) (group (+ not-newline)) eol)))
(defvar dtdcmp-cache-index-file-name "index.txt")
(defvar dtdcmp-cache-index-file-buffer-name " *dtdcmp index*")
(defvar dtdcmp-cache-count 0)
(defvar dtdcmp-dfenv (cc:dataflow-environment))
(defvar dtdcmp-dtd-cache-hash (make-hash-table :test 'equal))


(defun dtdcmp-get-work-buffer-name (dtd)
  (format " *dtdcmp %s*" (dtdcmp-dtd-url dtd)))

(defun dtdcmp-expand-in-buffer (buff dtd)
  (dtdcmp--trace "do expand in [ %s ]" buff)
  (let* ((entitynms (cl-loop for k being the hash-keys in (dtdcmp-dtd-entityh dtd)
                             collect k)))
    (when entitynms
      (with-current-buffer buff
        (cl-loop with re = (rx-to-string `(and "%" (group (or ,@entitynms)) ";"))
                 initially (goto-char (point-min))
                 while (re-search-forward re nil t)
                 do (let* ((entitynm (match-string-no-properties 1))
                           (entity (gethash entitynm (dtdcmp-dtd-entityh dtd))))
                      (cond ((dtdcmp-entity-p entity)
                             (replace-match (dtdcmp-entity-value entity)))
                            (t
                             (replace-match ""))))
                 do (goto-char (point-min)))))))

(defun dtdcmp-remove-commet-in-buffer (buff)
  (dtdcmp--trace "do remove comment in [ %s ]" buff)
  (yaxception:$
    (yaxception:try
      (with-current-buffer buff
        (cl-loop with res = (rx-to-string `(and "--"))
                 with ree = (rx-to-string `(and "--"))
                 initially (goto-char (point-min))
                 while (re-search-forward res nil t)
                 do (backward-char 2)
                 do (delete-char 2)
                 do (let* ((pts (point)))
                      (cond ((re-search-forward ree nil t) (delete-region pts (point)))
                            (t                             (delete-region pts (point-max))))))
        (cl-loop with re = (rx-to-string `(and "<!>" (* (any "\n"))))
                 initially (goto-char (point-min))
                 while (re-search-forward re nil t)
                 do (replace-match ""))))
    (yaxception:catch 'error e
      (dtdcmp--log-dump "Stack Trace" (yaxception:get-stack-trace e))
      (yaxception:throw 'dtdcmp-parse-error :desc "remove comment" :orgmsg (yaxception:get-text e)))))

(defvar dtdcmp-regexp-correct-url (rx-to-string `(and (+ space) "PUBLIC" (+ space)
                                                      "\"" (group (+ (not (any "\"")))) "\"" (+ space)
                                                      "\"" (group (+ (not (any "\"")))) "\"")))
(defun dtdcmp-correct-url-in-buffer (buff parenturl)
  (dtdcmp--trace "correct url in [ %s ]" buff)
  (yaxception:$
    (yaxception:try
      (with-current-buffer buff
        (cl-loop initially (goto-char (point-min))
                 while (re-search-forward dtdcmp-regexp-correct-url nil t)
                 do (let* ((extdesc (match-string-no-properties 1))
                           (exturl (match-string-no-properties 2))
                           (mdata (match-data))
                           (exturl (or (when (string-match "^https?://" exturl)
                                         exturl)
                                       (concat (replace-regexp-in-string "/[^/]+$" "/" parenturl) exturl))))
                      (set-match-data mdata)
                      (replace-match (format " PUBLIC \"%s\" \"%s\"" extdesc exturl))))))
    (yaxception:catch 'error e
      (dtdcmp--log-dump "Stack Trace" (yaxception:get-stack-trace e))
      (yaxception:throw 'dtdcmp-parse-error :desc "correct url" :orgmsg (yaxception:get-text e)))))

(defvar dtdcmp-regexp-simple-entity (rx-to-string `(and "<!" (* space) "ENTITY" (+ space)
                                                        "%" (+ space) (group (regexp ,dtdcmp-regexp-entitynm)) (+ space)
                                                        "\"" (group (* (not (any "\"")))) "\""
                                                        (* space) ">" (* "\n"))))
(defun dtdcmp-simple-entity-in-buffer (buff dtd)
  (dtdcmp--trace "do simple entity in [ %s ]" buff)
  (yaxception:$
    (yaxception:try
      (with-current-buffer buff
        (cl-loop initially (goto-char (point-min))
                 while (re-search-forward dtdcmp-regexp-simple-entity nil t)
                 do (let* ((entitynm (match-string-no-properties 1))
                           (entityvalue (match-string-no-properties 2))
                           (entity (make-dtdcmp-entity :name entitynm :value entityvalue)))
                      (replace-match "")
                      (dtdcmp--trace "found entity name:[%s] value:[%s] in [ %s ]"
                                     entitynm
                                     (replace-regexp-in-string "\n" " " entityvalue)
                                     buff)
                      (puthash entitynm entity (dtdcmp-dtd-entityh dtd))))
        ;; expand entity
        (dtdcmp-expand-in-buffer buff dtd)))
    (yaxception:catch 'error e
      (dtdcmp--log-dump "Stack Trace" (yaxception:get-stack-trace e))
      (yaxception:throw 'dtdcmp-parse-error :desc "simple entity" :orgmsg (yaxception:get-text e)))))

(defvar dtdcmp-regexp-remove-include (rx-to-string `(and "<!" (* space) "[" (* space) "INCLUDE" (* space) "["
                                                         (* space) (group (+ (not (any "]")))) (* space)
                                                         "]" (* space) "]" (* space) ">" (* "\n"))))
(defun dtdcmp-remove-include-in-buffer (buff dtd)
  (dtdcmp--trace "do remove include in [ %s ]" buff)
  (yaxception:$
    (yaxception:try
      (with-current-buffer buff
        (cl-loop initially (goto-char (point-min))
                 while (re-search-forward dtdcmp-regexp-remove-include nil t)
                 do (let* ((dtdtext (match-string-no-properties 1)))
                      (replace-match dtdtext)))))
    (yaxception:catch 'error e
      (dtdcmp--log-dump "Stack Trace" (yaxception:get-stack-trace e))
      (yaxception:throw 'dtdcmp-parse-error :desc "remove include" :orgmsg (yaxception:get-text e)))))

(defvar dtdcmp-regexp-remove-ignore (rx-to-string `(and "<!" (* space) "[" (* space) "IGNORE" (* space) "["
                                                        (* space) (+ (not (any "]"))) (* space)
                                                        "]" (* space) "]" (* space) ">" (* "\n"))))
(defun dtdcmp-remove-ignore-in-buffer (buff dtd)
  (dtdcmp--trace "do remove ignore in [ %s ]" buff)
  (yaxception:$
    (yaxception:try
      (with-current-buffer buff
        (cl-loop initially (goto-char (point-min))
                 while (re-search-forward dtdcmp-regexp-remove-ignore nil t)
                 do (replace-match ""))))
    (yaxception:catch 'error e
      (dtdcmp--log-dump "Stack Trace" (yaxception:get-stack-trace e))
      (yaxception:throw 'dtdcmp-parse-error :desc "remove ignore" :orgmsg (yaxception:get-text e)))))

(defvar dtdcmp-regexp-external-url (rx-to-string `(and "<!" (* space) "ENTITY" (+ space)
                                                       "%" (+ space) (group (regexp ,dtdcmp-regexp-entitynm)) (+ space)
                                                       "PUBLIC" (+ space) "\"" (+ (not (any "\""))) "\"" (+ space)
                                                       "\"" (group (+ (not (any "\"")))) "\""
                                                       (* space) ">" (* "\n"))))
(defun dtdcmp-get-external-alist-in-buffer (buff dtd)
  (dtdcmp--trace "get external alist in [ %s ]" buff)
  (yaxception:$
    (yaxception:try
      (with-current-buffer buff
        (cl-loop initially (goto-char (point-min))
                 while (re-search-forward dtdcmp-regexp-external-url nil t)
                 collect (let* ((entitynm (match-string-no-properties 1))
                                (exturl (match-string-no-properties 2)))
                           (replace-match "")
                           (cons exturl entitynm)))))
    (yaxception:catch 'error e
      (dtdcmp--log-dump "Stack Trace" (yaxception:get-stack-trace e))
      (yaxception:throw 'dtdcmp-parse-error :desc "get external alist" :orgmsg (yaxception:get-text e)))))

(defvar dtdcmp-regexp-remained-entity (rx-to-string `(and "<!" (* space) "ENTITY" (+ space)
                                                          "%" (+ space) (group (regexp ,dtdcmp-regexp-entitynm)) (+ space)
                                                          (group (+ (not (any ">"))))
                                                          (* space) ">" (* "\n"))))
(defun dtdcmp-remained-entity-in-buffer (buff dtd)
  (dtdcmp--trace "do remained entity in [ %s ]" buff)
  (yaxception:$
    (yaxception:try
      (with-current-buffer buff
        (cl-loop initially (goto-char (point-min))
                 while (re-search-forward dtdcmp-regexp-remained-entity nil t)
                 do (let* ((entitynm (match-string-no-properties 1))
                           (entityvalue (match-string-no-properties 2))
                           (entity (make-dtdcmp-entity :name entitynm :value entityvalue)))
                      (replace-match "")
                      (dtdcmp--trace "found entity name:[%s] value:[%s] in [ %s ]"
                                     entitynm
                                     (replace-regexp-in-string "\n" " " entityvalue)
                                     buff)
                      (puthash entitynm entity (dtdcmp-dtd-entityh dtd))))
        ;; expand entity
        (dtdcmp-expand-in-buffer buff dtd)))
    (yaxception:catch 'error e
      (dtdcmp--log-dump "Stack Trace" (yaxception:get-stack-trace e))
      (yaxception:throw 'dtdcmp-parse-error :desc "remained entity" :orgmsg (yaxception:get-text e)))))

(defvar dtdcmp-regexp-available-entity (rx-to-string `(and "<!" (* space) "ENTITY" (+ space)
                                                           (group (regexp ,dtdcmp-regexp-entitynm)) (+ space)
                                                           "\"" (group (* (not (any "\"")))) "\""
                                                           (* space) ">" (* "\n"))))
(defun dtdcmp-available-entity-in-buffer (buff dtd)
  (dtdcmp--trace "do available entity in [ %s ]" buff)
  (yaxception:$
    (yaxception:try
      (with-current-buffer buff
        (cl-loop initially (goto-char (point-min))
                 while (re-search-forward dtdcmp-regexp-available-entity nil t)
                 do (let* ((entitynm (match-string-no-properties 1))
                           (entityvalue (match-string-no-properties 2))
                           (entity (make-dtdcmp-entity :name entitynm :value entityvalue :available t)))
                      (replace-match "")
                      (dtdcmp--trace "got available entity name:[%s] value:[%s]" entitynm entityvalue)
                      (puthash entitynm entity (dtdcmp-dtd-entityh dtd))))))
    (yaxception:catch 'error e
      (dtdcmp--log-dump "Stack Trace" (yaxception:get-stack-trace e))
      (yaxception:throw 'dtdcmp-parse-error :desc "available entity" :orgmsg (yaxception:get-text e)))))

(defvar dtdcmp-regexp-element (rx-to-string `(and "<!" (* space) "ELEMENT" (+ space)
                                                  (group (+ (not (any space)))) (+ space) (group (+ (not (any ">"))))
                                                  (* space) ">" (* "\n"))))
(defun dtdcmp-element-in-buffer (buff dtd)
  (dtdcmp--trace "do element in [ %s ]" buff)
  (yaxception:$
    (yaxception:try
      (with-current-buffer buff
        (cl-loop initially (goto-char (point-min))
                 while (re-search-forward dtdcmp-regexp-element nil t)
                 do (let* ((tagnm (match-string-no-properties 1))
                           (tagvalue (match-string-no-properties 2)))
                      (replace-match "")
                      (dtdcmp--trace "start build tag name:[%s] value: %s" tagnm tagvalue)
                      (dtdcmp-build-tag dtd tagnm (replace-regexp-in-string "\n" " " tagvalue)))
                 do (sit-for 1))))
    (yaxception:catch 'error e
      (dtdcmp--log-dump "Stack Trace" (yaxception:get-stack-trace e))
      (yaxception:throw 'dtdcmp-parse-error :desc "element" :orgmsg (yaxception:get-text e)))))

(defvar dtdcmp-regexp-attlist (rx-to-string `(and "<!" (* space) "ATTLIST" (+ space)
                                                  (group (+ (not (any space)))) (+ space) (group (+ (not (any ">"))))
                                                  (* space) ">" (* "\n"))))
(defun dtdcmp-attlist-in-buffer (buff dtd)
  (dtdcmp--trace "do attlist in [ %s ]" buff)
  (yaxception:$
    (yaxception:try
      (with-current-buffer buff
        (cl-loop initially (goto-char (point-min))
                 while (re-search-forward dtdcmp-regexp-attlist nil t)
                 do (let* ((tagnm (match-string-no-properties 1))
                           (attrvalue (match-string-no-properties 2)))
                      (replace-match "")
                      (dtdcmp--trace "start build attr tagnm:[%s] value: %s" tagnm attrvalue)
                      (dtdcmp-build-attr dtd tagnm (replace-regexp-in-string "\n" " " attrvalue)))
                 do (sit-for 1))))
    (yaxception:catch 'error e
      (dtdcmp--log-dump "Stack Trace" (yaxception:get-stack-trace e))
      (yaxception:throw 'dtdcmp-parse-error :desc "attlist" :orgmsg (yaxception:get-text e)))))

(defvar dtdcmp-regexp-tag-opt (rx-to-string `(and bos (* space) (group (or "-" "O")) (+ space) (group (or "-" "O")))))
(defun dtdcmp-build-tag (dtd tagnm tagvalue)
  (let* ((tag (or (gethash tagnm (dtdcmp-dtd-tagh dtd))
                  (puthash tagnm (make-dtdcmp-tag :name tagnm) (dtdcmp-dtd-tagh dtd)))))
    (cond ((string-match "\\`\\s-*EMPTY\\s-*\\'" tagvalue)
           (setf (dtdcmp-tag-validate-re tag) "\\`\\'")
           (dtdcmp--trace "got tag validate-re of [%s] is [%s]" tagnm (dtdcmp-tag-validate-re tag)))
          ((string-match "\\`\\s-*ANY\\s-*\\'" tagvalue)
           (setf (dtdcmp-tag-validate-re tag) "\\`.*\\'")
           (setf (dtdcmp-tag-anychild tag) t)
           (setf (dtdcmp-tag-hastext tag) t)
           (dtdcmp--trace "got tag validate-re of [%s] is [%s]" tagnm (dtdcmp-tag-validate-re tag))
           (dtdcmp--trace "got tag anychild of [%s] is t" tagnm)
           (dtdcmp--trace "got tag hastext of [%s] is t" tagnm))
          ((string-match "\\`\\s-*(" tagvalue)
           (setq tagvalue (replace-regexp-in-string "\\s-+\\+(.+\\'" "" tagvalue))
           (setq tagvalue (replace-regexp-in-string "\\s-+-(.+\\'" "" tagvalue))
           (setq tagvalue (replace-regexp-in-string " " "" tagvalue))
           (when (string-match "#PCDATA" tagvalue)
             (setf (dtdcmp-tag-hastext tag) t)
             (setq tagvalue (replace-regexp-in-string "#PCDATA" "" tagvalue))
             (setq tagvalue (replace-regexp-in-string "(|" "(" tagvalue))
             (setq tagvalue (replace-regexp-in-string "(," "(" tagvalue)))
           (let* ((s tagvalue)
                  (re tagvalue))
             (setq s (replace-regexp-in-string "[()*+?]" "" s))
             (setq s (replace-regexp-in-string "[|&]" "," s))
             (setf (dtdcmp-tag-children tag) (split-string s ","))
             (dtdcmp--trace "got tag children of [%s] is [%s]" tagnm (mapconcat 'identity (dtdcmp-tag-children tag) ","))
             ;; (setq re (replace-regexp-in-string "(" "\\(" re))
             ;; (setq re (replace-regexp-in-string ")" "\\)" re))
             ;; (setq re (replace-regexp-in-string "|" "\\|" re))
             ;; (setf (dtdcmp-tag-validate-re tag) (concat "\\`" re "\\'"))))
             (setf (dtdcmp-tag-validate-re tag) (concat "\\`.*\\'"))
             (dtdcmp--trace "got tag validate-re of [%s] is [%s]" tagnm (dtdcmp-tag-validate-re tag))))
          ((string-match dtdcmp-regexp-tag-opt tagvalue)
           (let* ((startopt (match-string-no-properties 1 tagvalue))
                  (endopt (match-string-no-properties 2 tagvalue)))
             (setf (dtdcmp-tag-abbr-start tag) (string= startopt "O"))
             (setf (dtdcmp-tag-abbr-end tag) (string= endopt "O"))
             (dtdcmp--trace "got tag abbr-start of [%s] is [%s]" tagnm (dtdcmp-tag-abbr-start tag))
             (dtdcmp--trace "got tag abbr-end of [%s] is [%s]" tagnm (dtdcmp-tag-abbr-end tag))
             (dtdcmp-build-tag dtd tagnm (replace-regexp-in-string dtdcmp-regexp-tag-opt "" tagvalue))))
          (t
           (dtdcmp--error "failed build tag name:[%s] value: %s" tagnm tagvalue)))))

(defvar dtdcmp-regexp-attr-type (regexp-opt '("CDATA" "ID" "IDREF" "IDREFS" "ENTITY" "ENTITIES" "NMTOKEN" "NMTOKENS")))
(defvar dtdcmp-regexp-attr-def1 (rx-to-string `(and bos (* space) (group (+ (not (any space)))) (+ space)
                                                    (group (regexp ,dtdcmp-regexp-attr-type)))))
(defvar dtdcmp-regexp-attr-def2 (rx-to-string `(and bos (* space) (group (+ (not (any space)))) (+ space)
                                                    "(" (group (+ (not (any ")")))) ")" (+ space))))
(defun dtdcmp-build-attr (dtd tagnm attrvalue)
  (if (string-match "\\`(" tagnm)
      (progn (setq tagnm (replace-regexp-in-string "(" "" tagnm))
             (setq tagnm (replace-regexp-in-string ")" "" tagnm))
             (cl-loop for e in (split-string tagnm "|")
                      do (dtdcmp-build-attr dtd e attrvalue)))
    (let* ((tag (gethash tagnm (dtdcmp-dtd-tagh dtd))))
      (when (dtdcmp-tag-p tag)
        (cl-loop for re in (list dtdcmp-regexp-attr-def1 dtdcmp-regexp-attr-def2)
                 if (string-match re attrvalue)
                 return (let* ((attrnm (match-string-no-properties 1 attrvalue))
                               (attrtype (match-string-no-properties 2 attrvalue))
                               (attr (or (gethash attrnm (dtdcmp-tag-attrh tag))
                                         (puthash attrnm (make-dtdcmp-attr :name attrnm) (dtdcmp-tag-attrh tag)))))
                          (when (not (string-match dtdcmp-regexp-attr-type attrtype))
                            (setf (dtdcmp-attr-cands attr) (cl-loop for e in (split-string attrtype "|")
                                                                    for e = (replace-regexp-in-string "^\\s-+" "" e)
                                                                    for e = (replace-regexp-in-string "\\s-+$" "" e)
                                                                    collect e))
                            (dtdcmp--trace "got attr cands of [%s]@[%s] is [%s]" tagnm attrnm (mapconcat 'identity (dtdcmp-attr-cands attr) ",")))
                          (setf (dtdcmp-attr-validate-func attr) (cl-case attrtype
                                                                   ("CDATA"    'dtdcmp-validate-attr-cdata)
                                                                   ("ID"       'dtdcmp-validate-attr-cdata)
                                                                   ("IDREF"    'dtdcmp-validate-attr-cdata)
                                                                   ("IDREFS"   'dtdcmp-validate-attr-cdata)
                                                                   ("ENTITY"   'dtdcmp-validate-attr-cdata)
                                                                   ("ENTITIES" 'dtdcmp-validate-attr-cdata)
                                                                   ("NMTOKEN"  'dtdcmp-validate-attr-cdata)
                                                                   ("NMTOKENS" 'dtdcmp-validate-attr-cdata)
                                                                   (t          'dtdcmp-validate-attr-member)))
                          (dtdcmp-build-attr-opt dtd tagnm attr (replace-regexp-in-string re "" attrvalue)))
                 finally return (when (string-match "[^ ]" attrvalue)
                                  (dtdcmp--error "failed build attr tagnm:[%s] value: %s" tagnm attrvalue)))))))

(defun dtdcmp-validate-attr-cdata (x attr)
  (string-match ".*" x))

(defun dtdcmp-validate-attr-member (x attr)
  (string-match (regexp-opt (dtdcmp-attr-cands attr)) x))

(defvar dtdcmp-regexp-attr-opt (rx-to-string `(and bos (* space) (group (or "#REQUIRED" "#IMPLIED" "#FIXED")))))
(defun dtdcmp-build-attr-opt (dtd tagnm attr attrvalue)
  (when (string-match dtdcmp-regexp-attr-opt attrvalue)
    (let* ((attropt (match-string-no-properties 1 attrvalue)))
      (setf (dtdcmp-attr-required attr) (string= attropt "#REQUIRED"))
      (setf (dtdcmp-attr-editable attr) (not (string= attropt "#FIXED")))
      (setq attrvalue (replace-regexp-in-string dtdcmp-regexp-attr-opt "" attrvalue))
      (dtdcmp--trace "got attr required of [%s]@[%s] is [%s]" tagnm (dtdcmp-attr-name attr) (dtdcmp-attr-required attr))
      (dtdcmp--trace "got attr editable of [%s]@[%s] is [%s]" tagnm (dtdcmp-attr-name attr) (dtdcmp-attr-editable attr))))
  (dtdcmp-build-attr-default dtd tagnm attr attrvalue))

(defvar dtdcmp-regexp-string1 (rx-to-string `(and bos (* space) "\"" (group (* (not (any "\"")))) "\"")))
(defvar dtdcmp-regexp-string2 (rx-to-string `(and bos (* space) "'" (group (* (not (any "'")))) "'")))
(defun dtdcmp-build-attr-default (dtd tagnm attr attrvalue)
  (cl-loop for re in (list dtdcmp-regexp-string1 dtdcmp-regexp-string2)
           if (string-match re attrvalue)
           return (progn (setf (dtdcmp-attr-default attr) (match-string-no-properties 1 attrvalue))
                         (setq attrvalue (replace-regexp-in-string re "" attrvalue))
                         (dtdcmp--trace "got attr default of [%s]@[%s] is [%s]" tagnm (dtdcmp-attr-name attr) (dtdcmp-attr-default attr))))
  (dtdcmp-build-attr dtd tagnm attrvalue))

(defun dtdcmp-build-dtd (url &optional force)
  (lexical-let* ((url url))
    (dtdcmp-load-dtd-sentinel url force)
    (deferred:$
      (cc:dataflow-get dtdcmp-dfenv url)
      (deferred:nextc it
        (lambda (dtd)
          (dtdcmp-build-dtd-sentinel dtd)))
      (deferred:error it
        (lambda (e)
          (dtdcmp-finish-build-dtd (gethash url dtdcmp-dtd-cache-hash) 'failed (format "%s" e)))))))

(defun dtdcmp-load-dtd-sentinel (url &optional force)
  (let* ((dtd (or (when (not force)
                    (gethash url dtdcmp-dtd-cache-hash))
                  (puthash url (make-dtdcmp-dtd :url url :state 'initialized) dtdcmp-dtd-cache-hash))))
    (yaxception:$
      (yaxception:try
        (dtdcmp--trace "start loading url:[ %s ] force:[ %s ]" url force)
        (cc:dataflow-clear dtdcmp-dfenv url)
        (cl-case (dtdcmp-dtd-state dtd)
          (initialized  (setf (dtdcmp-dtd-state dtd) 'loading)
                        (or (when (not force)
                              (dtdcmp-load-dtd-from-cache dtd))
                            (dtdcmp-load-dtd-by-download dtd)))
          (loading      (dtdcmp--debug "already started loading : %s" url))
          (building     (dtdcmp--debug "already started building : %s" url))
          (t            (cc:dataflow-set dtdcmp-dfenv url dtd))))
      (yaxception:catch 'error e
        (dtdcmp-finish-build-dtd dtd 'failed (yaxception:get-text e))))))

(defun dtdcmp-load-dtd-from-cache (dtd)
  (dtdcmp--trace "try load from local cache : %s" (dtdcmp-dtd-url dtd))
  (dtdcmp-init-cache-directory)
  (let* ((dtdfile (dtdcmp-get-dtd-cache-file-path (dtdcmp-dtd-url dtd)))
         (dtdbuff (when (and (stringp dtdfile)
                             (file-exists-p dtdfile))
                    (find-file-noselect dtdfile))))
    (cond ((buffer-live-p dtdbuff)
           (with-current-buffer dtdbuff
             (setf (dtdcmp-dtd-text dtd) (buffer-string))
             (set-buffer-modified-p nil)
             (kill-buffer))
           (dtdcmp-finish-build-dtd dtd 'loaded)
           t)
          (t
           (dtdcmp--trace "not found or failed open cache file:[ %s ] url:[ %s ]" dtdfile (dtdcmp-dtd-url dtd))
           nil))))

(defvar dtdcmp-regexp-http-status (rx-to-string `(and bos "HTTP/" (+ (any "0-9.")) (+ space) (group (+ (any "0-9"))))))
(defvar dtdcmp-regexp-http-location (rx-to-string `(and bol "Location:" (+ space) (group (+ not-newline)) eol)))
(defun dtdcmp-load-dtd-by-download (dtd &optional redirecturl)
  (lexical-let* ((dtd dtd)
                 (url (or redirecturl
                          (dtdcmp-dtd-url dtd))))
    (dtdcmp--trace "try load by download : %s" url)
    (if (not (url-generic-parse-url url))
        (dtdcmp-finish-build-dtd dtd 'failed "invalid url")
      (message "Downloading '%s' ..." url)
      (deferred:$
        (deferred:timeout dtdcmp-download-timeout nil (deferred:url-retrieve url))
        (deferred:nextc it
          (lambda (buff)
            (if (not (buffer-live-p buff))
                (dtdcmp-finish-build-dtd dtd 'failed "timeout downloading")
              (with-current-buffer buff
                (dtdcmp--debug "downloaded [ %s ]\n%s" url (buffer-string))
                (goto-char (point-min))
                (let* ((httpstat (or (when (re-search-forward dtdcmp-regexp-http-status nil t)
                                       (string-to-number (match-string-no-properties 1)))
                                     0)))
                  (cond ((and (>= httpstat 200)
                              (<  httpstat 300))
                         (message "Download '%s' successful." url)
                         (goto-char (point-min))
                         (when (re-search-forward "\n\n" nil t)
                           (dtdcmp--trace "delete http header of [ %s ]" url)
                           (delete-region (point-min) (point)))
                         (dtdcmp-remove-commet-in-buffer buff)
                         (dtdcmp-correct-url-in-buffer buff url)
                         (setf (dtdcmp-dtd-text dtd) (buffer-string))
                         (dtdcmp-store-dtd-cache (dtdcmp-dtd-url dtd) buff)
                         (dtdcmp-finish-build-dtd dtd 'loaded))
                        ((and (>= httpstat 300)
                              (<  httpstat 400)
                              (goto-char (point-min))
                              (re-search-forward "\n\n" nil t)
                              (re-search-backward dtdcmp-regexp-http-location nil t))
                         (let* ((redirecturl (match-string-no-properties 1)))
                           (dtdcmp--trace "redirected [ %s ] to [ %s ]" url redirecturl)
                           (setf (dtdcmp-dtd-redirecturl dtd) redirecturl)
                           (dtdcmp-load-dtd-by-download dtd redirecturl)))
                        (t
                         (dtdcmp-finish-build-dtd dtd 'failed (format "failed http status : %s" httpstat))
                         (set-buffer-modified-p nil)
                         (kill-buffer))))))))
        (deferred:error it
          (lambda (e)
            (dtdcmp-finish-build-dtd dtd 'failed (format "%s" e))))))))

(defun dtdcmp-build-dtd-sentinel (dtd)
  (yaxception:$
    (yaxception:try
      (dtdcmp--trace "start building : %s" (dtdcmp-dtd-url dtd))
      (let* ((buff (get-buffer-create (dtdcmp-get-work-buffer-name dtd))))
        (cc:dataflow-clear dtdcmp-dfenv (dtdcmp-dtd-url dtd))
        (cl-case (dtdcmp-dtd-state dtd)
          (loaded       (setf (dtdcmp-dtd-state dtd) 'building)
                        (with-current-buffer buff
                          (erase-buffer)
                          (insert (dtdcmp-dtd-text dtd)))
                        (dtdcmp--trace "start build buffer:[%s]" buff)
                        (dtdcmp-build-dtd-buffer buff dtd))
          (building     (dtdcmp--debug "already started building : %s" (dtdcmp-dtd-url dtd)))
          (loading      nil)
          (initialized  nil)
          (t            (cc:dataflow-set dtdcmp-dfenv (dtdcmp-dtd-url dtd) dtd)))))
    (yaxception:catch 'error e
      (dtdcmp-finish-build-dtd dtd 'failed (yaxception:get-text e)))))

(defun dtdcmp-build-dtd-buffer (buff dtd)
  (yaxception:$
    (yaxception:try
      (dtdcmp-simple-entity-in-buffer buff dtd)
      (dtdcmp-remove-include-in-buffer buff dtd)
      (dtdcmp-remove-ignore-in-buffer buff dtd)
      (let* ((extalist (dtdcmp-get-external-alist-in-buffer buff dtd)))
        (dtdcmp--trace "collect external dtd in [ %s ]\n%s" buff extalist)
        (cond (extalist
               ;; load external dtd
               (cl-loop for e in extalist
                        for url = (car e)
                        do (dtdcmp-load-dtd-sentinel url))
               (lexical-let* ((dtd dtd)
                              (buff buff)
                              (extalist extalist))
                 (deferred:$
                   (deferred:parallel-list
                     (cl-loop for e in extalist
                              for url = (car e)
                              collect (cc:dataflow-get dtdcmp-dfenv url)))
                   (deferred:nextc it
                     (lambda (extdtds)
                       ;; regist entity
                       (cl-loop for extdtd in extdtds
                                for url = (when (dtdcmp-dtd-p extdtd)
                                            (dtdcmp-dtd-url extdtd))
                                if (dtdcmp-dtd-loaded-p url)
                                do (let* ((dtdtext (dtdcmp-dtd-text extdtd))
                                          (entitynm (assoc-default url extalist))
                                          (entity (make-dtdcmp-entity :name entitynm :value dtdtext)))
                                     (puthash entitynm entity (dtdcmp-dtd-entityh dtd))))
                       (dtdcmp--trace "start re build buffer:[%s]" buff)
                       (sit-for 2)
                       (dtdcmp-build-dtd-buffer buff dtd)))
                   (deferred:error it
                     (lambda (e)
                       (dtdcmp-finish-build-dtd dtd 'failed (format "%s" e)))))))
              (t
               ;; none external dtd
               (dtdcmp-remained-entity-in-buffer buff dtd)
               (dtdcmp-available-entity-in-buffer buff dtd)
               (sit-for 2)
               (dtdcmp-element-in-buffer buff dtd)
               (dtdcmp-attlist-in-buffer buff dtd)
               (dtdcmp-finish-build-dtd dtd 'available)))))
    (yaxception:catch 'error e
      (dtdcmp-finish-build-dtd dtd 'failed (yaxception:get-text e)))))

(defun dtdcmp-finish-build-dtd (dtd state &optional errmsg)
  (when (dtdcmp-dtd-p dtd)
    (dtdcmp--info "built [ %s ]. state is [ %s ]" (dtdcmp-dtd-url dtd) (symbol-name state))
    (setf (dtdcmp-dtd-state dtd) state)
    (when (eq state 'failed)
      (message "Failed get DTD from '%s' : %s." (dtdcmp-dtd-url dtd) errmsg)
      (dtdcmp--error "can't available [ %s ] : %s" (dtdcmp-dtd-url dtd) errmsg)
      (setf (dtdcmp-dtd-errmsg dtd) errmsg)
      (dtdcmp-remove-dtd-cache (dtdcmp-dtd-url dtd)))
    (let* ((buff (get-buffer (dtdcmp-get-work-buffer-name dtd))))
      (when (buffer-live-p buff)
        (with-current-buffer buff
          (dtdcmp--debug "kill buffer:[%s]. remaining ...\n%s" buff (buffer-string))
          (set-buffer-modified-p nil)
          (kill-buffer))))
    (cc:dataflow-set dtdcmp-dfenv (dtdcmp-dtd-url dtd) dtd)
    (dtdcmp--trace "finish build : %s" (dtdcmp-dtd-url dtd))))

(defun dtdcmp-dtd-available-p (url)
  (let* ((dtd (gethash url dtdcmp-dtd-cache-hash)))
    (and (dtdcmp-dtd-p dtd)
         (eq (dtdcmp-dtd-state dtd) 'available))))

(defun dtdcmp-dtd-loaded-p (url)
  (let* ((dtd (gethash url dtdcmp-dtd-cache-hash)))
    (and (dtdcmp-dtd-p dtd)
         (or (eq (dtdcmp-dtd-state dtd) 'available)
             (eq (dtdcmp-dtd-state dtd) 'loaded)))))

(defun dtdcmp-get-dtd (url)
  (gethash url dtdcmp-dtd-cache-hash))

(defun dtdcmp-init-cache-directory (&optional force)
  (when (and force
             (file-directory-p dtdcmp-dtd-cache-directory))
    (dtdcmp--info "delete cache in [ %s ]" dtdcmp-dtd-cache-directory)
    (cl-loop for filenm in (directory-files dtdcmp-dtd-cache-directory)
             for f = (concat dtdcmp-dtd-cache-directory "/" filenm)
             when (file-regular-p f)
             do (delete-file f)))
  (when (not (file-directory-p dtdcmp-dtd-cache-directory))
    (dtdcmp--trace "mkdir [ %s ]" dtdcmp-dtd-cache-directory)
    (make-directory dtdcmp-dtd-cache-directory)))

(defun dtdcmp-get-dtd-cache-file-path (url)
  (let* ((idx (dtdcmp-get-dtd-cache-index url))
         (ret (when (stringp idx)
                (concat dtdcmp-dtd-cache-directory "/" idx ".txt"))))
    (dtdcmp--trace "got cache path [ %s ] for [ %s ]" ret url)
    ret))

(defun dtdcmp-get-dtd-index-file-buffer ()
  (or (get-buffer dtdcmp-cache-index-file-buffer-name)
      (let* ((idxfile (concat dtdcmp-dtd-cache-directory "/" dtdcmp-cache-index-file-name))
             (idxexist (file-exists-p idxfile))
             (idxbuff (find-file-noselect idxfile)))
        (with-current-buffer idxbuff
          (when (not idxexist)
            (dtdcmp--trace "not yet exist index file [ %s ]" idxfile)
            (insert "\n")
            (save-buffer))
          (rename-buffer dtdcmp-cache-index-file-buffer-name))
        idxbuff)))

(defun dtdcmp-get-dtd-cache-index (url)
  (let* ((idx (with-current-buffer (dtdcmp-get-dtd-index-file-buffer)
                (cl-loop initially (goto-char (point-min))
                         for line = (replace-regexp-in-string "[\0\r\n]" "" (thing-at-point 'line))
                         until (eobp)
                         when (and (string-match dtdcmp-regexp-cache-line line)
                                   (string= (match-string-no-properties 2 line) url))
                         return (match-string-no-properties 1 line)
                         do (forward-line 1)
                         finally return nil))))
    (dtdcmp--trace "got index [ %s ] cache of [ %s ]" idx url)
    idx))

(defun dtdcmp-store-dtd-cache (url buff)
  (dtdcmp--trace "start store cache of [ %s ]" url)
  (yaxception:$
    (yaxception:try
      (let* ((idx (dtdcmp-get-dtd-cache-index url)))
        (when (not (stringp idx))
          (dtdcmp--trace "start append index entry of [ %s ]" url)
          (setq idx (format "%d" (setq dtdcmp-cache-count (+ dtdcmp-cache-count 1))))
          (with-current-buffer (dtdcmp-get-dtd-index-file-buffer)
            (goto-char (point-max))
            (insert idx " " url "\n")
            (save-buffer))
          (dtdcmp--trace "appended index entry of [ %s ]" url))
        (with-current-buffer buff
          (write-file (concat dtdcmp-dtd-cache-directory "/" idx ".txt") nil)
          (set-buffer-modified-p nil)
          (kill-buffer)))
      (dtdcmp--info "stored cache of [ %s ]" url))
    (yaxception:catch 'error e
      (yaxception:throw 'dtdcmp-parse-error :desc "store DTD" :orgmsg (yaxception:get-text e)))))

(defun dtdcmp-remove-dtd-cache (url)
  (let* ((dtdfile (dtdcmp-get-dtd-cache-file-path url)))
    (when (and (stringp dtdfile)
               (file-exists-p dtdfile))
      (dtdcmp--trace "delete [ %s ]" dtdfile)
      (delete-file dtdfile))))

(defun dtdcmp-load-cache-count ()
  (dtdcmp--trace "start load cache count")
  (dtdcmp-init-cache-directory)
  (with-current-buffer (dtdcmp-get-dtd-index-file-buffer)
    (setq dtdcmp-cache-count (cl-loop with maxidx = 0
                                      initially (goto-char (point-min))
                                      for line = (replace-regexp-in-string "[\0\r\n]" "" (thing-at-point 'line))
                                      until (eobp)
                                      when (string-match dtdcmp-regexp-cache-line line)
                                      do (let* ((curridx (string-to-number (match-string-no-properties 1 line))))
                                           (when (> curridx maxidx)
                                             (setq maxidx curridx)))
                                      finally return maxidx
                                      do (forward-line 1))))
  (dtdcmp--trace "found cache count [ %s ]" dtdcmp-cache-count))


(defvar dtdcmp-regexp-jump-current-tag-start (rx-to-string `(and (group (or "<" ">")) (+ (not (any ">"))))))
(defun dtdcmp-point-inside-tag-p ()
  (save-excursion
    (and (re-search-backward dtdcmp-regexp-jump-current-tag-start nil t)
         (string= (match-string-no-properties 1) "<"))))

(defvar dtdcmp-regexp-doctype (rx-to-string `(and "<!DOCTYPE" (+ space) (group (+ (not (any space)))) (+ space)
                                                  "PUBLIC" (+ space) "\"" (+ (not (any "\""))) "\"" (+ space)
                                                  "\"" (group (+ (not (any "\"")))) "\"" (* space) ">")))
(defvar dtdcmp-buffer-dtd-url nil)
(make-variable-buffer-local 'dtdcmp-buffer-dtd-url)
(defvar dtdcmp-buffer-root-tag nil)
(make-variable-buffer-local 'dtdcmp-buffer-root-tag)
(defun dtdcmp-update-current-dtd ()
  (let (url roottag)
    (save-excursion
      (goto-char (point-min))
      (cond ((re-search-forward dtdcmp-regexp-doctype nil t)
             (setq roottag (downcase (match-string-no-properties 1)))
             (setq url (match-string-no-properties 2)))
            (t
             (setq roottag dtdcmp-default-root-tag)
             (setq url dtdcmp-default-dtd-url))))
    (setq dtdcmp-buffer-root-tag roottag)
    (when (not (string= url dtdcmp-buffer-dtd-url))
      (dtdcmp--trace "start update current dtd to [ %s ]" url)
      (setq dtdcmp-buffer-dtd-url url)
      (dtdcmp-build-dtd url))))

(defvar dtdcmp-buffer-current-tag nil)
(make-variable-buffer-local 'dtdcmp-buffer-current-tag)
(defun dtdcmp-update-current-tag ()
  (save-excursion
    (let* ((tagnm ""))
      (when (re-search-backward "<[^/]" nil t)
        (forward-char)
        (let* ((start (point)))
          (skip-syntax-forward "w")
          (setq tagnm (buffer-substring-no-properties start (point)))))
      (setq dtdcmp-buffer-current-tag tagnm))))

(defvar dtdcmp-regexp-start-or-end-tag (rx-to-string `(and "<" (group (? "/")) (group (+ (not (any ">" space))))
                                                           (* (not (any ">")) ">"))))
(defvar dtdcmp-buffer-parent-tag nil)
(make-variable-buffer-local 'dtdcmp-buffer-parent-tag)
(defun dtdcmp-update-parent-tag (current-tag-start-point)
  (save-excursion
    (goto-char current-tag-start-point)
    (let* ((depth 1)
           (tagnm dtdcmp-buffer-root-tag))
      (while (and (> depth 0)
                  (re-search-backward dtdcmp-regexp-start-or-end-tag nil t))
        (cond ((string= (match-string-no-properties 1) "/") (incf depth))
              (t                                            (decf depth)))
        (setq tagnm (match-string-no-properties 2)))
      (setq dtdcmp-buffer-parent-tag tagnm))))

(defvar dtdcmp-regexp-point-inside-attr (rx-to-string `(and (+ space) (group (+ (not (any space)))) "=" (or "\"" "'")
                                                            (* (not (any "\"" "'"))) point)))
(defvar dtdcmp-buffer-current-attr nil)
(make-variable-buffer-local 'dtdcmp-buffer-current-attr)
(defun dtdcmp-update-current-attr ()
  (let* ((attrnm ""))
    (when (dtdcmp-point-inside-tag-p)
      (save-excursion
        (when (re-search-backward dtdcmp-regexp-point-inside-attr nil t)
          (setq attrnm (match-string-no-properties 1)))))
    (setq dtdcmp-buffer-current-attr attrnm)))

(defvar dtdcmp-regexp-point-cssprop-value (rx-to-string `(and (or "\"" "'" ";" space) (group (+ (any "a-zA-Z0-9-"))) ":" (* space)
                                                              (* (not (any ":" "\"" "'"))) point)))
(defvar dtdcmp-buffer-current-cssprop nil)
(make-variable-buffer-local 'dtdcmp-buffer-current-cssprop)
(defun dtdcmp-update-current-cssprop ()
  (let* ((propnm ""))
    (save-excursion
      (when (re-search-backward dtdcmp-regexp-point-cssprop-value nil t)
        (setq propnm (match-string-no-properties 1))))
    (setq dtdcmp-buffer-current-cssprop propnm)))

(defvar dtdcmp-regexp-point-tagnm (rx-to-string `(and "<" (* (not (any "/" ">" space))) point)))
(defvar dtdcmp-regexp-point-attrnm (rx-to-string `(and (or (and "<" (+ (any "a-zA-Z0-9:-")))
                                                           (and (not (any "=")) "\"")
                                                           (and (not (any "=")) "'"))
                                                       (+ space) (* (any "a-zA-Z0-9-")) point)))
(defvar dtdcmp-regexp-point-cssprop (rx-to-string `(and (or "\"" "'" ";") (* space) (* (any "a-zA-Z0-9-")) point)))
(defun dtdcmp-get-current-context-symbol ()
  (ignore-errors
    (save-excursion
      (cond ((re-search-backward dtdcmp-regexp-point-tagnm nil t)
             (dtdcmp-update-parent-tag (point))
             'tag)
            ((and (dtdcmp-point-inside-tag-p)
                  (save-excursion
                    (re-search-backward dtdcmp-regexp-point-attrnm nil t)))
             (dtdcmp-update-current-tag)
             'attr)
            ((and (dtdcmp-update-current-attr)
                  (not (string= dtdcmp-buffer-current-attr "")))
             (cond ((string= dtdcmp-buffer-current-attr "style")
                    (cond ((re-search-backward dtdcmp-regexp-point-cssprop nil t)
                           'cssprop)
                          (t
                           (dtdcmp-update-current-cssprop)
                           'csspropvalue)))
                   (t
                    (dtdcmp-update-current-tag)
                    'attrvalue)))
            (t
             'otherwise)))))

(defvar dtdcmp-buffer-point-cssprop nil)
(make-variable-buffer-local 'dtdcmp-buffer-point-cssprop)
(defvar dtdcmp-buffer-point-cssprop-value nil)
(make-variable-buffer-local 'dtdcmp-buffer-point-cssprop-value)
(defun dtdcmp-get-candidates-current-context ()
  (dtdcmp-update-current-dtd)
  (if (not (dtdcmp-dtd-available-p dtdcmp-buffer-dtd-url))
      (progn (message "[DTDCMP] Now loading %s ..." dtdcmp-buffer-dtd-url)
             nil)
    (let* ((dtd (dtdcmp-get-dtd dtdcmp-buffer-dtd-url))
           (ctx (dtdcmp-get-current-context-symbol)))
      (setq dtdcmp-buffer-point-cssprop nil)
      (setq dtdcmp-buffer-point-cssprop-value nil)
      (cl-case ctx
        (tag (let* ((parenttag (gethash dtdcmp-buffer-parent-tag (dtdcmp-dtd-tagh dtd))))
               (or (when (dtdcmp-tag-p parenttag)
                     (dtdcmp-tag-children parenttag))
                   (when (and (dtdcmp-tag-p parenttag)
                              (dtdcmp-tag-anychild parenttag))
                     (cl-loop for k being the hash-keys in (dtdcmp-dtd-tagh dtd)
                              collect k)))))
        (attr (let* ((tag (gethash dtdcmp-buffer-current-tag (dtdcmp-dtd-tagh dtd))))
                (when (dtdcmp-tag-p tag)
                  (cl-loop for k being the hash-keys in (dtdcmp-tag-attrh tag)
                           collect k))))
        (attrvalue (let* ((tag (gethash dtdcmp-buffer-current-tag (dtdcmp-dtd-tagh dtd)))
                          (attr (when (dtdcmp-tag-p tag)
                                  (gethash dtdcmp-buffer-current-attr (dtdcmp-tag-attrh tag)))))
                     (when (dtdcmp-attr-p attr)
                       (dtdcmp-attr-cands attr))))
        (cssprop (progn (setq dtdcmp-buffer-point-cssprop t)
                        (dtdcmp-get-css-properties)))
        (csspropvalue (progn (setq dtdcmp-buffer-point-cssprop-value t)
                             (setq ac-css-property dtdcmp-buffer-current-cssprop)
                             (ac-css-property-candidates)))))))

(defun dtdcmp-get-available-entities ()
  (dtdcmp-update-current-dtd)
  (if (not (dtdcmp-dtd-available-p dtdcmp-buffer-dtd-url))
      (progn (message "[DTDCMP] Now loading %s ..." dtdcmp-buffer-dtd-url)
             nil)
    (let* ((dtd (dtdcmp-get-dtd dtdcmp-buffer-dtd-url)))
      (cl-loop for e being the hash-values in (dtdcmp-dtd-entityh dtd)
               if (and (dtdcmp-entity-p e)
                       (dtdcmp-entity-available e))
               collect (dtdcmp-entity-name e)))))

(defun dtdcmp-get-css-properties ()
  (cl-loop for prop in ac-css-property-alist
           collect (car prop)))

(defun dtdcmp-get-document-tag (selected)
  (ignore-errors
    (when (stringp selected)
      (set-text-properties 0 (string-width selected) nil selected)
      "Not documented.")))

(defun dtdcmp-get-document-attr (selected)
  (ignore-errors
    (when (stringp selected)
      (set-text-properties 0 (string-width selected) nil selected)
      "Not documented.")))

(defun dtdcmp-get-document-entity (selected)
  (ignore-errors
    (when (stringp selected)
      (set-text-properties 0 (string-width selected) nil selected)
      (let* ((dtd (dtdcmp-get-dtd dtdcmp-buffer-dtd-url))
             (entity (when (dtdcmp-dtd-p dtd)
                       (gethash selected (dtdcmp-dtd-entityh dtd)))))
        (cond ((dtdcmp-entity-p entity)
               (concat selected " is ENTITY.\n\n"
                       "Value : " (dtdcmp-entity-value entity)))
              (t
               "Not documented."))))))


(defvar ac-source-dtdcmp-tag
  '((candidates . dtdcmp-get-candidates-current-context)
    (prefix . "<\\([a-zA-Z0-9:-]*\\)")
    (symbol . "t")
    (document . dtdcmp-get-document-tag)
    (requires . 0)
    (cache)
    (limit . nil)
    (action . (lambda ()
                (let* ((currpt (point))
                       (dtd (dtdcmp-get-dtd dtdcmp-buffer-dtd-url))
                       (tagnm (save-excursion
                                (skip-syntax-backward "w")
                                (buffer-substring-no-properties (point) currpt)))
                       (tag (when (dtdcmp-dtd-p dtd)
                              (gethash tagnm (dtdcmp-dtd-tagh dtd))))
                       (abbr-end (when (dtdcmp-tag-p tag)
                                   (dtdcmp-tag-abbr-end tag))))
                  (when (string= (format "%c" (char-after)) "/")
                    (delete-char 1))
                  (when (string= (format "%c" (char-after)) ">")
                    (delete-char 1))
                  (cond (abbr-end (insert "/>"))
                        (tagnm    (insert (format "></%s>" tagnm)))
                        (t        (insert ">")))
                  (goto-char currpt))))))

(defvar ac-source-dtdcmp-attr
  '((candidates . dtdcmp-get-candidates-current-context)
    (prefix . "\\(?:<[a-zA-Z0-9:-]+\\|[^=]\"\\|[^=]'\\)\\s-+\\([a-zA-Z0-9-]*\\)")
    (symbol . "a")
    (document . dtdcmp-get-document-attr)
    (requires . 0)
    (cache)
    (limit . nil)
    (action . (lambda ()
                (insert "=\"")
                (when (string= (format "%c" (char-after)) "\"")
                  (delete-char 1))
                (insert "\"")
                (backward-char)
                (auto-complete)))))

(defvar ac-source-dtdcmp-attr-value
  '((candidates . dtdcmp-get-candidates-current-context)
    (prefix . "\\(?:=\"\\|='\\|\:\\|;\\)\\s-*\\([^\"':; ]*\\)")
    (symbol . "v")
    (requires . 0)
    (cache)
    (limit . nil)
    (action . (lambda ()
                (cond (dtdcmp-buffer-point-cssprop
                       (insert ": ")
                       (auto-complete '(ac-source-css-property)))
                      (dtdcmp-buffer-point-cssprop-value
                       (insert ";")))))))

(defvar ac-source-dtdcmp-entity
  '((candidates . dtdcmp-get-available-entities)
    (prefix . "&\\([a-zA-Z0-9_.-]*\\)")
    (symbol . "e")
    (document . dtdcmp-get-document-entity)
    (requires . 0)
    (cache)
    (limit . nil)
    (action . (lambda ()
                (insert ";")))))


(defun dtdcmp-self-insert-with-ac-trigger-command (n)
  (interactive "p")
  (self-insert-command n)
  (ac-trigger-key-command n))

(defun dtdcmp-clear-cache ()
  (interactive)
  (setq dtdcmp-dtd-cache-hash (make-hash-table :test 'equal))
  (dtdcmp-init-cache-directory t))

(defun dtdcmp-setup ()
  (interactive)
  (yaxception:regist-trace-prefix "dtdcmp-")
  (yaxception:$
    (yaxception:try
      (dtdcmp--log-set-level 'fatal 'trace)
      (dtdcmp--trace "start setup")
      (local-set-key (kbd "SPC") 'dtdcmp-self-insert-with-ac-trigger-command)
      (add-to-list 'ac-sources 'ac-source-dtdcmp-tag)
      (add-to-list 'ac-sources 'ac-source-dtdcmp-attr)
      (add-to-list 'ac-sources 'ac-source-dtdcmp-attr-value)
      (add-to-list 'ac-sources 'ac-source-dtdcmp-entity)
      (add-to-list 'ac-modes major-mode)
      (auto-complete-mode)
      (dtdcmp-load-cache-count)
      (dtdcmp-update-current-dtd))
    (yaxception:catch 'error e
      (dtdcmp--fatal "failed setup : %s" (yaxception:get-text e))
      (dtdcmp--log-open-log-if-debug))))


(provide 'dtd-completion)
