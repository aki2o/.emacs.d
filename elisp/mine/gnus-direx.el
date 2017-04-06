
(eval-when-compile (require 'cl))
(require 'rx)
(require 'gnus)
(require 'gnus-group)
(require 'gnus-ez-lib)
(require 'direx)
(require 'log4e)
(require 'yaxception)

(defgroup gnus-direx nil
  "Gnus Group Explorer."
  :group 'gnus
  :prefix "gnus-direx:")

(defcustom gnus-direx::regexp-tree-delimiter "[/.]"
  "String as the regular expression for the delimiter to make group be tree."
  :type 'regexp
  :group 'gnus-direx)

(defcustom gnus-direx:select-group-command 'gnus-group-select-group
  "Function for the choice of group."
  :type 'function
  :group 'gnus-direx)

(defcustom gnus-direx:auto-update-p t
  "Whether update automatically with the sync to the `gnus-group-mode' buffer."
  :type 'boolean
  :group 'gnus-direx)

(defcustom gnus-direx:prior-p t
  "Whether display prior than the `gnus-group-mode' buffer."
  :type 'boolean
  :group 'gnus-direx)

(defface gnus-direx:node-face
  '((t (:inherit dired-directory)))
  "Face for the package part of explorer."
  :group 'gnus-direx)

(defface gnus-direx:unread-group-face
  nil
  "Face for the regular method part of explorer."
  :group 'gnus-direx)

(defface gnus-direx:read-group-face
  '((t (:inherit dired-ignored)))
  "Face for the regular method part of explorer."
  :group 'gnus-direx)


(log4e:deflogger "gnus-direx" "%t [%l] %m" "%H:%M:%S" '((fatal . "fatal")
                                                        (error . "error")
                                                        (warn  . "warn")
                                                        (info  . "info")
                                                        (debug . "debug")
                                                        (trace . "trace")))
(gnus-direx--log-set-level 'trace)

(yaxception:deferror 'gnus-direx:unavailable-function nil "Not exist available function in gnus")


(defvar gnus-direx::group-buffer-name "*Group*")


;;;;;;;;;;;;;
;; Utility

(defmacro gnus-direx::awhen (test &rest body)
  (declare (indent 1))
  `(let ((it ,test)) (when it ,@body)))

(defun* gnus-direx::show-message (msg &rest args)
  (apply 'message (concat "[GNUS-DIREX] " msg) args)
  nil)

(defun gnus-direx::get-parent-node-names (grpfullnm)
  (gnus-direx--trace "start get parent node names : %s" grpfullnm)
  (let ((prefix (or (gnus-group-real-prefix grpfullnm) "")))
    (if (string= prefix "")
        (gnus-direx--warn "can't get prefix of %s" grpfullnm)
      (let* ((rootnm (replace-regexp-in-string ":\\'" "" prefix))
             (childnms (loop with parentnm = rootnm
                             with delim = ":"
                             with remain = (substring grpfullnm (length prefix))
                             while (> (length remain) 0)
                             if (string-match gnus-direx::regexp-tree-delimiter remain)
                             collect (let* ((beginpt (match-beginning 0))
                                            (endpt (match-end 0))
                                            (childnm (substring remain 0 beginpt))
                                            (ret (concat parentnm delim childnm)))
                                       (setq parentnm ret)
                                       (setq delim (substring remain beginpt endpt))
                                       (setq remain (substring remain endpt))
                                       ret)
                             else
                             collect (let ((ret (concat parentnm delim remain)))
                                       (setq remain "")
                                       ret))))
        (loop for e in `(,rootnm ,@childnms)
              if (and e (not (string= e grpfullnm)))
              collect e)))))

(defsubst gnus-direx::get-group-display (shortnm total unread status)
  (let ((status (or (when (string-match "%S" gnus-group-line-format)
                      status)
                    ""))
        (unread (cond ((and (numberp unread) (>= unread 0)) unread)
                      (t                                    "*"))))
    (format "%s (%s/%s) %s" shortnm unread total status)))

(defsubst gnus-direx::get-bottom-name (fullnm &optional parentnms)
  (let* ((parentnms (or parentnms
                        (gnus-direx::get-parent-node-names fullnm)))
         (delim (if (> (length parentnms) 1)
                    gnus-direx::regexp-tree-delimiter
                  (regexp-quote ":")))
         (ret (substring fullnm (length (car (last parentnms)))))
         (ret (replace-regexp-in-string (concat "\\`" delim) "" ret)))
    ret))

(defsubst gnus-direx::get-group-face (unread status)
  (cond ((and (numberp unread)
              (> unread 0))
         'gnus-direx:unread-group-face)
        (t
         'gnus-direx:read-group-face)))


;;;;;;;;;;;
;; Trees

(defgeneric gnus-direx::get-full-name (tree)
  "Return the string of TREE.")

(defclass gnus-direx::group (direx:leaf)
  ((fullnm :initarg :full-name
           :accessor gnus-direx::group-full-name)
   (shortnm :initarg :short-name
            :accessor gnus-direx::group-short-name)
   (total :initarg :total
          :accessor gnus-direx::group-total-articles)
   (unread :initarg :unread
           :accessor gnus-direx::group-unread-articles)
   (status :initarg :status
           :accessor gnus-direx::group-status)
   (updatep :accessor gnus-direx::group-updatep)
   (orignm :initarg :orig-name
           :accessor gnus-direx::group-original-name)))

(defclass gnus-direx::node (direx:node)
  ((fullnm :initarg :full-name
           :accessor gnus-direx::node-full-name)
   (shortnm :initarg :short-name
            :accessor gnus-direx::node-short-name)
   (belongs :initarg :belongs
            :accessor gnus-direx::node-belongs)
   (rootp :initarg :rootp
          :accessor gnus-direx::node-rootp)
   (protocol :initarg :protocol
             :accessor gnus-direx::node-protocol)
   (server :initarg :server
           :accessor gnus-direx::node-server)))

(defsubst gnus-direx::append-belong-group (node grp)
  (let ((curr (gnus-direx::node-belongs node)))
    (setf (gnus-direx::node-belongs node) (if (not curr)
                                              (list grp)
                                            (append curr (list grp))))))

(defmethod direx:tree-equals ((x gnus-direx::group) y)
  (or (eq x y)
      (and (typep y 'gnus-direx::group)
           (equal (gnus-direx::group-full-name x) (gnus-direx::group-full-name y)))))

(defmethod gnus-direx::get-full-name ((grp gnus-direx::group))
  (gnus-direx::group-full-name grp))

(defmethod direx:tree-equals ((x gnus-direx::node) y)
  (or (eq x y)
      (and (typep y 'gnus-direx::node)
           (equal (gnus-direx::node-full-name x) (gnus-direx::node-full-name y)))))

(defmethod direx:node-children ((node gnus-direx::node))
  nil)

(defmethod direx:node-contains ((node gnus-direx::node) tree)
  (let ((s1 (gnus-direx::node-full-name node))
        (s2 (gnus-direx::get-full-name tree)))
    (and s1
         s2
         (direx:starts-with s2 s1))))

(defmethod gnus-direx::get-full-name ((node gnus-direx::node))
  (gnus-direx::node-full-name node))


;;;;;;;;;;;;;;;;
;; Tree Items

(defgeneric gnus-direx::need-refresh-item-p (item)
  "Return whether the update of ITEM is need.")

(defclass gnus-direx::node-item (direx:item)
  ((updatep :accessor gnus-direx::node-item-updatep)))

(defclass gnus-direx::group-item (direx:item) ())

(defvar gnus-direx::proxy-prefix "gnus-direx:do-")
(defmethod direx:generic-find-item ((item gnus-direx::group-item) not-this-window)
  (let ((cmd (intern-soft (concat gnus-direx::proxy-prefix
                                  (symbol-name gnus-direx:select-group-command)))))
    (if (not (commandp cmd))
        (gnus-direx::show-message "Can't execute %s" cmd)
      (call-interactively cmd))))

(defmethod gnus-direx::need-refresh-item-p ((item gnus-direx::node-item))
  (gnus-direx::node-item-updatep item))

(defun gnus-direx::sort-item-from-fact (i1 i2)
  (let ((pt1 (direx:item-start i1))
        (pt2 (direx:item-start i2)))
    (if (< pt1 pt2) t nil)))

(defmethod direx:item-refresh ((item gnus-direx::node-item))
  (gnus-direx--trace "start direx:item-refresh : %s"
                     (gnus-direx::node-full-name (direx:item-tree item)))
  (setf (direx:item-children item)
        (sort (direx:item-children item) 'gnus-direx::sort-item-from-fact))
  (setf (gnus-direx::node-item-updatep item) nil))

(defmethod direx:make-item ((node gnus-direx::node) parent)
  (make-instance 'gnus-direx::node-item
                 :tree node
                 :parent parent
                 :face 'gnus-direx:node-face))


(defmethod gnus-direx::need-refresh-item-p ((item gnus-direx::group-item))
  (gnus-direx::group-updatep (direx:item-tree item)))

(defmethod direx:item-refresh ((item gnus-direx::group-item))
  (let* ((grp (direx:item-tree item))
         (shortnm (gnus-direx::group-short-name grp))
         (total (gnus-direx::group-total-articles grp))
         (unread (gnus-direx::group-unread-articles grp))
         (status (gnus-direx::group-status grp)))
    (gnus-direx--trace "start direx:item-refresh : %s" (gnus-direx::group-full-name grp))
    (setf (direx:tree-name grp)
          (gnus-direx::get-group-display shortnm total unread status))
    (setf (gnus-direx::group-updatep grp) nil)
    (setf (direx:item-face item) (gnus-direx::get-group-face unread status))
    (direx:item-delete item)
    (direx:item-insert item)))

(defmethod direx:make-item ((grp gnus-direx::group) parent)
  (make-instance 'gnus-direx::group-item
                 :tree grp
                 :parent parent
                 :face (gnus-direx::get-group-face (gnus-direx::group-unread-articles grp)
                                                   (gnus-direx::group-status grp))))


;;;;;;;;;;;;;;;;;;
;; Setup Buffer

(defun gnus-direx::get-current-group-original-name ()
  (let* ((item (direx:item-at-point))
         (tree (when item (direx:item-tree item))))
    (when (typep tree 'gnus-direx::group)
      (gnus-direx::group-original-name tree))))

(defun gnus-direx::move-to-group (grpnm)
  (when grpnm
    (loop initially (goto-char (point-min))
          while (not (eobp))
          for currgrpnm = (gnus-group-group-name)
          for dgrpnm = (when currgrpnm (gnus-group-decoded-name currgrpnm))
          if (and dgrpnm (string= dgrpnm grpnm))
          return t
          do (forward-line))))

(defsubst gnus-direx::def-proxy (cmdnm orgcmd)
  (gnus-direx--trace "start def proxy. cmdnm[%s] orgcmd[%s]" cmdnm orgcmd)
  (eval `(defun ,(intern cmdnm) ()
           (interactive)
           (gnus-direx--trace "do %s" (symbol-name ',orgcmd))
           (let ((grpnm (gnus-direx::get-current-group-original-name)))
             (with-current-buffer (get-buffer gnus-direx::group-buffer-name)
               (if (not (gnus-direx::move-to-group grpnm))
                   (gnus-direx::show-message "Can't find %s in %s" grpnm gnus-direx::group-buffer-name)
                 (call-interactively ',orgcmd)))))))

(defsubst gnus-direx::get-sequence-key-strokes (basestr startch endch)
  (gnus-direx--trace "start get sequence key strokes. basestr[%s] startch[%s] endch[%s]"
                     basestr startch endch)
  (loop for ch from (string-to-char startch) to (string-to-char endch)
        collect (concat basestr (make-string 1 ch))))

(defvar gnus-direx::regexp-sequence-key-stroke (rx-to-string `(and (group (not (any space)))
                                                                   " .. "
                                                                   (group (* not-newline))
                                                                   (group (not (any space)))
                                                                   eos)))
(defun gnus-direx::get-keys (orgmapsym &optional not-convert)
  (with-temp-buffer
    (let ((indent-tabs-mode t)
          (orgmap (symbol-value orgmapsym)))
      (insert (substitute-command-keys "\\<orgmap>\\{orgmap}"))
      (goto-char (point-min))
      (forward-line 3)
      (loop while (not (eobp))
            for e = (split-string (buffer-substring (point-at-bol) (point-at-eol)) "\t+")
            for keystr = (replace-regexp-in-string
                          "\\\"" "\\\\\"" (replace-regexp-in-string "\\\\" "\\\\\\\\" (pop e)))
            for orgcmd = (when e (intern-soft (pop e)))
            for cmdnm = (when (and orgcmd (commandp orgcmd))
                          (if not-convert
                              (symbol-name orgcmd)
                            (concat gnus-direx::proxy-prefix (symbol-name orgcmd))))
            for cmd = (when cmdnm (or (gnus-direx::awhen (intern-soft cmdnm)
                                        (when (commandp it) it))
                                      (gnus-direx::def-proxy cmdnm orgcmd)))
            if (and cmd (string-match gnus-direx::regexp-sequence-key-stroke keystr))
            append (loop for keystr in (gnus-direx::get-sequence-key-strokes
                                        (match-string-no-properties 2 keystr)
                                        (match-string-no-properties 1 keystr)
                                        (match-string-no-properties 3 keystr))
                         collect `(,keystr . ,cmd))
            else if (and keystr cmd)
            collect `(,keystr . ,cmd)
            do (forward-line)))))

(defun gnus-direx::make-keymap ()
  (gnus-direx--trace "start make keymap.")
  (let ((map (make-sparse-keymap)))
    (loop for keyinfo in (append (gnus-direx::get-keys 'gnus-group-mode-map)
                                 (gnus-direx::get-keys 'direx:direx-mode-map t))
          for keystr = (car keyinfo)
          for cmd = (cdr keyinfo)
          do (gnus-direx--trace "define key. stroke[%s] cmd[%s]" keystr cmd)
          do (define-key map (read-kbd-macro keystr) cmd))
    map))

(defvar gnus-direx::buffer-name "*DirEX Gnus*")
(defvar gnus-direx::root-items nil)
(defun gnus-direx::get-buffer ()
  (or (get-buffer gnus-direx::buffer-name)
      (with-current-buffer (generate-new-buffer gnus-direx::buffer-name)
        (direx:direx-mode)
        (set (make-local-variable 'gnus-direx::root-items) nil)
        (use-local-map (gnus-direx::make-keymap))
        (current-buffer))))


;;;;;;;;;;;;;;;;;;;
;; Update Buffer

(defsubst gnus-direx::make-node (fullnm)
  (let* ((rootp (not (string-match "\\`\\([^:]+\\):" fullnm)))
         (prefix (if rootp fullnm (match-string-no-properties 1 fullnm)))
         (prefixes (split-string prefix "\\+"))
         (protocol (when prefixes (pop prefixes)))
         (server (when prefixes (pop prefixes)))
         (shortnm (cond (rootp (or server protocol))
                        (t     (gnus-direx::get-bottom-name fullnm))))
         (nodenm (cond ((and rootp server) (format "%s [%s]" shortnm protocol))
                       (t                  shortnm))))
    (gnus-direx--trace "create node[%s]. nodenm[%s] protocol[%s] server[%s] rootp[%s]"
                       fullnm nodenm protocol server rootp)
    (make-instance 'gnus-direx::node
                   :name nodenm
                   :short-name shortnm
                   :full-name fullnm
                   :rootp rootp
                   :protocol protocol
                   :server server)))

(defsubst gnus-direx::get-new-tree-list (curr-treenms new-grpnms)
  (gnus-direx--trace "start get new tree list.")
  (let ((foundtrees))
    (with-temp-buffer
      (loop for treenm in curr-treenms
            do (insert treenm "\n")
            do (push treenm foundtrees))
      (loop for grpnm in new-grpnms
            do (loop for parentnm in (gnus-direx::get-parent-node-names grpnm)
                     if (not (member parentnm foundtrees))
                     do (progn
                          (insert parentnm "\n")
                          (push parentnm foundtrees)))
            do (insert grpnm "\n")
            do (push grpnm foundtrees))
      (sort-lines nil (point-min) (point-max))
      (delete "" (split-string (buffer-string) "\n+")))))

(defun gnus-direx::get-new-group-hash-with-update-tree (tree-hash)
  (gnus-direx--trace "start get new group hash with update tree.")
  (let ((buff (get-buffer gnus-direx::group-buffer-name))
        (ret (make-hash-table :test 'equal)))
    (when (and buff (buffer-live-p buff))
      (with-current-buffer buff
        (save-excursion
          (loop initially (goto-char (point-min))
                while (not (eobp))
                for grpnm = (gnus-group-group-name)
                if grpnm
                do (let* ((active (symbol-value (get-text-property (point-at-bol) 'gnus-group)))
                          (total (or (when active (1+ (- (cdr active) (car active)))) 0))
                          (unread (or (gnus-group-group-unread) 0))
                          (unread (if (not (numberp unread)) -1 unread))
                          (lvl (gnus-group-group-level))
                          (status (cond ((not lvl)                        "")
                                        ((<= lvl gnus-level-subscribed)   "")
                                        ((<= lvl gnus-level-unsubscribed) "U")
                                        ((= lvl gnus-level-zombie)        "Z")
                                        (t                                "K")))
                          (dgrpnm (gnus-group-decoded-name grpnm))
                          (fullnm (gnus-ez-lib:get-group-full-name
                                   (cond ((functionp 'gnus-group-guess-full-name)
                                          (gnus-group-guess-full-name dgrpnm))
                                         ((functionp 'gnus-group-guess-full-name-from-command-method)
                                          (gnus-group-guess-full-name-from-command-method dgrpnm))
                                         (t
                                          (yaxception:throw 'gnus-direx:unavailable-function)))))
                          (parentnms (gnus-direx::get-parent-node-names fullnm))
                          (shortnm (gnus-direx::get-bottom-name fullnm parentnms))
                          (currgrp (gethash fullnm tree-hash)))
                     (cond (currgrp
                            (gnus-direx--trace "found group[%s]" fullnm)
                            (when (and (typep currgrp 'gnus-direx::group)
                                       (or (not (= total (gnus-direx::group-total-articles currgrp)))
                                           (not (= unread (gnus-direx::group-unread-articles currgrp)))
                                           (not (string= status (gnus-direx::group-status currgrp)))))
                              (gnus-direx--trace "update prop of group[%s]" fullnm)
                              (setf (gnus-direx::group-original-name currgrp) dgrpnm)
                              (setf (gnus-direx::group-total-articles currgrp) total)
                              (setf (gnus-direx::group-unread-articles currgrp) unread)
                              (setf (gnus-direx::group-status currgrp) status)
                              (setf (gnus-direx::group-updatep currgrp) t))
                            ;; Remove the exist node/group from current hash
                            (remhash fullnm tree-hash)
                            (loop for parentnm in parentnms
                                  do (remhash parentnm tree-hash)))
                           (t
                            (gnus-direx--trace
                             "create group[%s]. shortnm[%s] total[%s] unread[%s] status[%s]"
                             fullnm shortnm total unread status)
                            (puthash fullnm
                                     (make-instance 'gnus-direx::group
                                                    :name (gnus-direx::get-group-display shortnm
                                                                                         total
                                                                                         unread
                                                                                         status)
                                                    :short-name shortnm
                                                    :full-name fullnm
                                                    :total total
                                                    :unread unread
                                                    :status status
                                                    :orig-name dgrpnm)
                                     ret))))
                do (forward-line)))))
    ret))

(defun gnus-direx::get-parent-item (tree item)
  (loop while item
        for currtree = (direx:item-tree item)
        if (direx:node-contains currtree tree)
        return item
        else
        do (setq item (direx:item-parent item))))

(defun gnus-direx:update-buffer ()
  (yaxception:$~
    (yaxception:try
      (gnus-direx--trace "start update buffer.")
      (with-current-buffer (gnus-direx::get-buffer)
        (save-excursion
          (save-restriction
            (widen)
            (let* ((curr-treeh (make-hash-table :test 'equal))
                   (curr-treenms (loop initially (goto-char (point-min))
                                       while (not (eobp))
                                       for item = (direx:item-at-point)
                                       collect (when item
                                                 (let* ((tree (direx:item-tree item))
                                                        (treenm (gnus-direx::get-full-name tree)))
                                                   (puthash treenm tree curr-treeh)
                                                   treenm))
                                       do (forward-line)))
                   (new-grph (gnus-direx::get-new-group-hash-with-update-tree curr-treeh))
                   (delete-treenms (loop for treenm being the hash-key in curr-treeh collect treenm))
                   (new-treenms (gnus-direx::get-new-tree-list
                                 (loop for treenm in curr-treenms
                                       if (and treenm
                                               (not (member treenm delete-treenms)))
                                       collect treenm)
                                 (loop for grpnm being the hash-key in new-grph collect grpnm))))
              (gnus-direx--trace "got new tree list.\n%s" (mapconcat 'identity new-treenms "\n"))
              (if (not new-treenms)
                  (let ((buffer-read-only nil))
                    (erase-buffer))
                (goto-char (point-min))
                (loop with newtreenm = (pop new-treenms)
                      with treenm = (when curr-treenms (pop curr-treenms))
                      with parent = nil
                      while newtreenm
                      for line = (1+ (count-lines (point-min) (point)))
                      if (not treenm)
                      ;; If empty line
                      do (progn
                           ;; Move cursol to next and current item comes next
                           (setq treenm (when curr-treenms (pop curr-treenms)))
                           (forward-line))
                      if (member treenm delete-treenms)
                      ;; If current item is the deleted tree
                      do (let* ((item (direx:item-at-point!))
                                (currparent (direx:item-parent item)))
                           ;; Delete the item and current item comes next
                           (gnus-direx--trace "at line[%s] delete tree : %s" line treenm)
                           (direx:item-delete item)
                           (when currparent
                             (setf (direx:item-children currparent)
                                   (delq item (direx:item-children currparent))))
                           (setq treenm (when curr-treenms (pop curr-treenms))))
                      else if (string= treenm newtreenm)
                      ;; If current item is next new item
                      do (let ((item (direx:item-at-point!)))
                           ;; New/current item comes next and move cursol to next
                           (gnus-direx--trace "at line[%s] no change" line)
                           (setq newtreenm (when new-treenms (pop new-treenms)))
                           (setq treenm (when curr-treenms (pop curr-treenms)))
                           (when (typep (direx:item-tree item) 'gnus-direx::node)
                             (setq parent item))
                           (forward-line))
                      else
                      ;; If next new item should be added
                      do (let* ((newtree (or (gethash newtreenm new-grph)
                                             (gnus-direx::make-node newtreenm)))
                                (currparent (when (or (typep newtree 'gnus-direx::group)
                                                      (not (gnus-direx::node-rootp newtree)))
                                              ;; If new item is group or not root node, that needs parent
                                              (gnus-direx::get-parent-item newtree parent)))
                                (newitem (direx:make-item newtree currparent)))
                           ;; Insert the item and new item comes next
                           (gnus-direx--trace "at line[%s] insert tree : %s" line newtreenm)
                           (direx:item-insert newitem)
                           (direx:item-expand newitem)
                           (setq newtreenm (when new-treenms (pop new-treenms)))
                           ;; If parent exists, add the reference of child to parent
                           (when currparent
                             (let* ((children (direx:item-children currparent))
                                    (children (if children
                                                  (append children (list newitem))
                                                (list newitem))))
                               (setf (direx:item-children currparent) children)
                               (setf (gnus-direx::node-item-updatep currparent) t)
                               (when (not (direx:item-open currparent))
                                 (direx:item-hide newitem))))
                           ;; Update parent
                           (when (typep newtree 'gnus-direx::node)
                             (setq parent newitem))))
                (gnus-direx--trace "start refresh item")
                (goto-char (point-min))
                (while (not (eobp))
                  (gnus-direx::awhen (direx:item-at-point)
                    (when (gnus-direx::need-refresh-item-p it)
                      (direx:item-refresh it)))
                  (forward-line)))))))
      (gnus-direx--trace "finished update buffer."))
    (yaxception:catch 'error e
      (gnus-direx::show-message "Failed update buffer : %s" (yaxception:get-text e))
      (gnus-direx--error "failed update buffer : %s" (yaxception:get-text e)))))

(defun gnus-direx:popup-buffer ()
  (switch-to-buffer (gnus-direx::get-buffer)))


;;;;;;;;;;;;;;;;;;;;;;
;; Behavior of Gnus

(defmacro gnus-direx:def-update-advice (func)
  (declare (indent 0))
  `(defadvice ,func (after gnus-direx:update activate)
     (when gnus-direx:auto-update-p
       (gnus-direx:update-buffer))))

(defmacro gnus-direx:def-popup-advice (func)
  (declare (indent 0))
  `(defadvice ,func (after gnus-direx:popup activate)
     (when gnus-direx:prior-p
       (gnus-direx:popup-buffer))))

(defun gnus-direx:default-setup ()
  "Do the default setup of behavior for Gnus."
  (add-hook 'gnus-group-prepare-hook 'gnus-direx:update-buffer t)

  (gnus-direx:def-update-advice gnus-group-update-group)
  (gnus-direx:def-update-advice gnus-group-update-group-line)
  (gnus-direx:def-update-advice gnus-group-kill-group)
  (gnus-direx:def-update-advice gnus-group-make-group)
  (gnus-direx:def-update-advice gnus-group-yank-group)

  (gnus-direx:def-popup-advice gnus)
  (gnus-direx:def-popup-advice gnus-server-exit)
  (gnus-direx:def-popup-advice gnus-summary-exit)
  (gnus-direx:def-popup-advice gnus-summary-exit-no-update)
  (gnus-direx:def-popup-advice gnus-batch-score)
  (gnus-direx:def-popup-advice gnus-other-frame)
  )


(provide 'gnus-direx)
