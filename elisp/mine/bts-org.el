
(eval-when-compile (require 'cl))
(require 'bts)


;; * org-mode configuration


(defvar bts-org::special-properties
  '("PROPERTIES" "END" "TODO" "TAGS" "ALLTAGS" "CATEGORY" "PRIORITY"
    "DEADLINE" "SCHEDULED" "CLOSED" "TIMESTAMP" "TIMESTAMP_IA" "CLOCKSUM"
    "BLOCKED" "ITEM" "FILE" "COLUMNS" "CATEGORIES" "ARCHIVE" "LOG_INTO_DRAWER"
    "LOGGING" "STYLE" "LAST_REPEAT" "COOKIE_DATA" "ORDERED" "BLOCKER"))

;; property name like "xxx_ALL" is not avialable.


(defun bts-org:make-project-view ()
  (bts--trace "start org:regist project.")
  (let* ((lo `((:type text :name path :label "OrgFilePath" :size 60 :require t) BR BR
               ;; "[ Select Use Functions ]" BR
               ;; ,@(bts-org::gen-builtin-tmpl) BR BR
               "[ Define Custom Properties ]" BR
               bts:widget-expand-flex-layout))
         (opts '(("Text"     . text)
                 ("CheckBox" . chkbox)
                 ("Radio"    . radio)
                 ("Select"   . select)
                 ("Date"     . date)))
         (flo `((:type text :name name :label "Name" :size 15 :require t)
                (:type select :name type :label "Type" :options ,opts :require t :action on-update)
                (bts-org::gen-optional-tmpl))))
    `(:layout ,lo :flex-layout ,flo :submit-action bts-org::submit-project-register)))

(defvar bts-org::builtin-functions
  '((todo . nil)
    (tags . nil)
    (priority . nil)
    (deadline . nil)
    (scheduled . nil)
    (timestamp . nil)
    (clock . nil)
    (subtree . ((:type checkbox :name subtree-ordered :prefix "Use Order")))
    (checkbox . nil)
    (logging . nil)
    (habits . nil)
    (archive . nil)
    ))

(defun bts-org::gen-builtin-tmpl ()
  (loop for (name . plists) in bts-org::builtin-functions
        for lbl = (upcase (symbol-name name))
        collect `(:type checkbox :name ,name :label ,lbl :prefix "Use")
        append (loop for e in plists
                     collect " "
                     collect e)
        collect 'BR))

(defun bts-org::gen-builtin-defaults ()
    (loop for (name . plists) in bts-org::builtin-functions
          collect (cons name t)
          append (loop for e in plists
                       for k = (plist-get e :name)
                       for v = (plist-get e :default)
                       if v collect (cons k v))))

(defun bts-org::gen-optional-tmpl ()
  (let* ((idx (bts:widget-get-flex-current-index))
         (type (bts:widget-get-flex-value 'type idx)))
    (case type
      (text   '((:type text :name size :label "Size" :size 4 :value "30")
                (:type checkbox :name require :prefix "Require")
                (:type checkbox :name secret :prefix "Secret")))
      (chkbox '((:type text :name text :label "Text" :size 20 :value "Yes")
                (:type checkbox :name tick :prefix "Tick")))
      (radio  '(:type text :name cands :label "Elements" :size 30))
      (select '((:type text :name cands :label "Elements" :size 30)
                (:type checkbox :name require :prefix "Require")
                (:type checkbox :name multiple :prefix "Multiple"))))))

(defun bts-org::submit-project-register (mdl)
  (let ((cprops (loop for m in (bts:widget-get-flex-models mdl)
                      for type = (assoc-default 'type m)
                      for cargs = `(:name ,(assoc-default 'name m) :label ,(assoc-default 'label m))
                      if (eq type 'text)
                      collect (let ((size (assoc-default 'size m))
                                    (req (assoc-default 'require m))
                                    (secret (assoc-default 'secret m)))
                                `(:type text ,@cargs :size ,size :require ,req :secret ,secret))
                      else if (eq type 'chkbox)
                      collect (let ((suffix (assoc-default 'text m))
                                    (tick (assoc-default 'tick m)))
                                `(:type checkbox ,@cargs :suffix ,suffix :tick ,tick))
                      else if (memq type '(radio select))
                      collect (let* ((cvalue (or (assoc-default 'cands m) ""))
                                     (opts (-map 's-trim (split-string cvalue ",")))
                                     (radio (eq type 'radio))
                                     (req (assoc-default 'require m))
                                     (multi (assoc-default 'multiple m)))
                                `(:type select ,@cargs :options ,opts :radio ,radio :require ,req :multiple ,multi))
                      else if (eq type 'date)
                      collect `(:type date ,@cargs :separator "-"))))
    `(:path ,(assoc-default 'path mdl) :customs ,cprops)))

(defun bts-org:make-query-view ()
  )

(defun bts-org:regist-ticket (tickets)
  )

(defun bts-org:fetch-ticket (project query)
  )

(defun bts-org:make-ticket-single-view (project)
  )

(defun bts-org:get-ticket-summary (project ticket)
  )


(bts:system-regist
 (make-bts:system
  :name               'org
  :project-view       'bts-org:make-project-view
  :query-view         'bts-org:make-query-view
  :ticket-single-view 'bts-org:make-ticket-single-view
  :ticket-fetcher     'bts-org:fetch-ticket
  :ticket-register    'bts-org:regist-ticket
  :ticket-summary     'bts-org:get-ticket-summary))


(provide 'bts-org)
;;; bts-org.el ends here
