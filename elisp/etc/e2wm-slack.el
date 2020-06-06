(require 'cl-lib)
(require 'e2wm)
(require 'slack)
(require 'direx-slack-room)
(require 'dash)
(require 'log4e)
(require 'emoji-cheat-sheet-plus nil t)


(defgroup e2wm-slack nil
  "Perspective of e2wm.el for work in slack."
  :group 'windows
  :prefix "e2wm-slack:")

(defcustom e2wm-slack:room-max-rows 1
  ""
  :type 'integer
  :group 'e2wm-slack)

(defcustom e2wm-slack:room-max-cols 2
  ""
  :type 'integer
  :group 'e2wm-slack)

(defcustom e2wm-slack:input-buffer-size-ratio 0.15
  ""
  :type 'number
  :group 'e2wm-slack)

(defcustom e2wm-slack:room-buffer-width 35
  ""
  :type 'number
  :group 'e2wm-slack)

(defcustom e2wm-slack:select-buffer-width 50
  ""
  :type 'number
  :group 'e2wm-slack)

(defcustom e2wm-slack:slack-buffer-font-decrease 3
  ""
  :type 'number
  :group 'e2wm-slack)

(defcustom e2wm-slack:select-commands '(slack-select-rooms
                                        slack-message-embed-mention
                                        slack-message-embed-channel
                                        slack-message-add-reaction
                                        emoji-cheat-sheet-plus-buffer
                                        emoji-cheat-sheet-plus-insert
                                        pophint:do-situationally-e2wm)
  ""
  :type '(repeat symbol)
  :group 'e2wm-slack)


(defface e2wm-slack:slack-active-buffer-header-face
  '((t (:background "white" :foreground "red")))
  "")

(defface e2wm-slack:slack-inactive-buffer-header-face
  '((t (:background "white" :foreground "blue")))
  "")


(log4e:deflogger "e2wm-slack" "%t [%l] %m" "%H:%M:%S" '((fatal . "fatal")
                                                        (error . "error")
                                                        (warn  . "warn")
                                                        (info  . "info")
                                                        (debug . "debug")
                                                        (trace . "trace")))
(e2wm-slack--log-set-level 'trace)


(defvar e2wm-slack::next-prior-slack-buffer nil)
(defvar e2wm-slack::pinned-room-ids nil)
(defvar e2wm-slack::active-slack-buffer nil)


;;;;;;;;;;;;;
;; Utility

(defsubst e2wm-slack:managed-p ()
  (e2wm:aif (e2wm:managed-p)
      (eq (e2wm:$pst-name it) 'slack)))

(defsubst e2wm-slack::slack-buffer-p (buf)
  (memq (buffer-local-value 'major-mode buf) '(slack-mode slack-info-mode)))

(defsubst e2wm-slack::get-buffer-if-live (buffer-name)
  (e2wm:aif (get-buffer buffer-name)
      (when (buffer-live-p it)
        it)))

(defsubst e2wm-slack::current-team-rooms ()
  (let ((team (slack-team-select)))
    (with-slots (groups ims channels) team
      (append ims groups channels))))

(defsubst e2wm-slack::get-team-and-room-of-buffer (&optional buf)
  (let* ((team-id (ignore-errors
                    (if buf
                        (buffer-local-value 'slack-current-team-id buf)
                      slack-current-team-id)))
         (room-id (ignore-errors
                    (if buf
                        (buffer-local-value 'slack-current-room-id buf)
                      slack-current-room-id)))
         (team (when team-id (slack-team-find team-id)))
         (room (when (and team room-id)
                 (slack-room-find room-id team))))
    (list team room)))

(defsubst e2wm-slack::get-room-identifier (team room)
  (when (and team room)
    (format "%s"
            (cond ((object-of-class-p room 'slack-room)
                   (slack-room-name room))
                  ((object-of-class-p room 'slack-im)
                   (slack-im-user-presence room))))))

(defun e2wm-slack::scale-fontsize (&optional increase-p)
  (let ((scale-func (if increase-p 'text-scale-increase 'text-scale-decrease)))
    (when (fboundp scale-func)
      (cl-loop for buf in (buffer-list)
               if (e2wm-slack::slack-buffer-p buf)
               do (with-current-buffer buf
                    (funcall scale-func e2wm-slack:slack-buffer-font-decrease))))))

(defmacro e2wm-slack::move-succeed-p (&rest body)
  (declare (indent 0))
  `(let ((pt (point)))
     ,@body
     (not (= (point) pt))))


;;;;;;;;;;;;;;;;;
;; Perspective

(e2wm:pst-class-register
 (make-e2wm:$pst-class :name    'slack
                       :extend  'array
                       :title   "Slack"
                       :main    'input
                       :init    'e2wm-slack:dp-init
                       :start   'e2wm-slack:dp-start
                       :leave   'e2wm-slack:dp-leave
                       :switch  'e2wm-slack:dp-switch
                       :popup   'e2wm-slack:dp-popup
                       :display 'e2wm-slack:dp-display
                       :update  'e2wm-slack:dp-update
                       :keymap  'e2wm-slack:dp-minor-mode-map))

(defun e2wm-slack:dp-init ()
  (let* ((buffers (e2wm-slack::get-next-slack-buffers))
         (wm (e2wm-slack::make-wm buffers)))
    (e2wm-slack::start-slack-if-not-yet)
    (ad-enable-regexp "\\`e2wm-slack:")
    (ad-activate-all)
    (e2wm:dp-array-arrange-buffers wm buffers)))

(defun e2wm-slack:dp-start (wm)
  (e2wm-slack::scale-fontsize))

(defun e2wm-slack:dp-leave (wm)
  (setq e2wm:prev-selected-buffer nil)
  (e2wm-slack::scale-fontsize t)
  (ad-disable-regexp "\\`e2wm-slack:")
  (ad-activate-all))

(defun e2wm-slack:dp-switch (buf)
  (e2wm:message "#DP SLACK switch : %s" buf)
  (e2wm-slack::dp-handle-buffer buf))

(defun e2wm-slack:dp-popup (buf)
  (e2wm:message "#DP SLACK popup : %s" buf)
  (e2wm-slack::dp-handle-buffer buf))

(defun e2wm-slack:dp-display (buf)
  (e2wm:message "#DP SLACK display : %s" buf)
  (e2wm-slack::dp-handle-buffer buf))

(defun e2wm-slack:dp-update (wm)
  (setq e2wm-slack::active-slack-buffer (wlf:get-buffer wm 'w-1-1))
  (e2wm-slack::update-slack-buffer-header (e2wm-slack::get-next-slack-buffers))
  (setq e2wm-slack::next-prior-slack-buffer nil)
  (e2wm:dp-base-update wm))


(defun e2wm-slack::dp-handle-buffer (buf)
  (e2wm-slack--log 'trace "start dp handle buffer. buf[%s]" buf)
  (cond ((member (buffer-name buf) (list slack-message-write-buffer-name
                                         slack-message-edit-buffer-name))
         (e2wm-slack--trace "set buffer as input")
         (e2wm:with-advice
          (e2wm-slack::input-setup-buffer buf)
          (e2wm:pst-buffer-set 'input buf t t))
         t)
        ((e2wm-slack::slack-buffer-p buf)
         (e2wm-slack--trace "set buffer as slack buffer")
         (setq e2wm-slack::next-prior-slack-buffer buf)
         (e2wm-slack::relayout-windows)
         (with-current-buffer buf
           (let ((slack-buffer-function (lambda (buf)
                                          (e2wm-slack::input-setup-buffer buf)
                                          (switch-to-buffer-other-window buf))))
             (slack-message-write-another-buffer))))
        ((memq this-command e2wm-slack:select-commands)
         (e2wm-slack--trace "set buffer as select")
         (e2wm:with-advice
          (e2wm:pst-buffer-set 'select buf t t))
         t)
        (t
         (e2wm-slack--trace "set buffer as sub")
         (e2wm:with-advice
          (e2wm:pst-buffer-set 'sub buf t nil)))))

(defvar e2wm-slack::previous-wm nil)

(defun e2wm-slack::restore-layout-windows ()
  (when e2wm-slack::previous-wm
    (let ((instance (e2wm:pst-get-instance)))
      (setf (e2wm:$pst-wm instance) e2wm-slack::previous-wm)
      (e2wm:pst-update-windows t))))

(defun e2wm-slack::relayout-windows (&optional buffers)
  (let* ((buffers (or buffers (e2wm-slack::get-next-slack-buffers)))
         (instance (e2wm:pst-get-instance))
         (wm (e2wm-slack::make-wm buffers)))
    (setq e2wm-slack::previous-wm wm)
    (e2wm:dp-array-arrange-buffers wm buffers)
    (setf (e2wm:$pst-wm instance) wm)
    (e2wm:pst-update-windows t)))

(defun e2wm-slack::make-wm (buffers)
  (let* ((size (e2wm-slack::calculate-window-size (length buffers)))
         (cols (car size))
         (rows (cdr size))
         (recipe (e2wm-slack::make-recipe cols rows))
         (wparams (e2wm-slack::make-winfo cols rows)))
    (e2wm-slack--trace "made recipe.\n%s" recipe)
    (e2wm-slack--trace "made winfo.\n%s" wparams)
    (wlf:no-layout recipe wparams)))

(defun e2wm-slack::calculate-window-size (num)
  (e2wm-slack--log 'trace "start calculate window size. num[%s]" num)
  (let* ((mod (mod num e2wm-slack:room-max-cols))
         (rows (/ num e2wm-slack:room-max-cols)))
    (when (> mod 0)
      (incf rows))
    (cons (min num e2wm-slack:room-max-cols)
          (min rows e2wm-slack:room-max-rows))))

(defun e2wm-slack::make-recipe (cols rows)
  (e2wm-slack--log 'trace "start make recipe. cols[%s] rows[%s]" cols rows)
  (let ((sz-array (- 1.0 e2wm-slack:input-buffer-size-ratio)))
    (decf cols)
    (cl-labels
        ((loop-rows (cols rows)
                    (cl-loop for i from rows downto 1
                             with ret = (loop-cols cols rows)
                             if (< i rows)
                             do (setq ret
                                      `(- (:upper-size-ratio ,(* sz-array (/ 1.0 (- rows (- i 1.5)))))
                                          ,(loop-cols cols i) ,ret))
                             finally return ret))
         (loop-cols (cols y)
                    (cl-loop for i from cols downto 1
                             with ret = (mk (1+ cols) y)
                             do (setq ret
                                      `(| (:left-size ,(/ (frame-width) (1+ cols)))
                                          ,(mk i y) ,ret))
                             finally return ret))
         (mk (x y)
             (intern (format "w-%i-%i" x y))))
      (let ((ar (cond ((and (< cols 1) (<= rows 1))
                       'w-1-1)
                      (t
                       (loop-rows cols rows)))))
        `(- (:upper-size-ratio ,sz-array)
            (| (:left-max-size ,e2wm-slack:select-buffer-width) select
               (| (:left-max-size ,e2wm-slack:room-buffer-width) rooms ,ar))
            (| (:left-size-ratio 0.6) input sub))))))

(defun e2wm-slack::make-winfo (cols rows)
  (e2wm-slack--log 'trace "start make winfo. cols[%s] rows[%s]" cols rows)
  (cl-labels
      ((loop-rows (cols rows)
                  (cl-loop for i from 1 to rows
                           with ret = nil
                           do (setq ret (nconc ret (loop-cols cols i)))
                           finally return ret))
       (loop-cols (cols y)
                  (cl-loop for i from 1 to cols
                           with ret = nil
                           do (setq ret (nconc ret (list (mk i y))))
                           finally return ret))
       (mk (x y)
           (list ':name (intern (format "w-%i-%i" x y)))))
    (let ((ar (cond ((and (< cols 1) (<= rows 1))
                     (list (mk 1 1)))
                    (t
                     (loop-rows cols rows)))))
      (add-to-list 'ar '(:name input :plugin slack-input) t)
      (add-to-list 'ar '(:name rooms :plugin slack-rooms) t)
      (add-to-list 'ar '(:name select :default-hide t) t)
      (add-to-list 'ar '(:name sub) t))))

(defun e2wm-slack::update-slack-buffer-header (buffers)
  (cl-loop for buf in buffers
           if (e2wm-slack::slack-buffer-p buf)
           do (with-current-buffer buf
                (destructuring-bind (team room) (e2wm-slack::get-team-and-room-of-buffer)
                  (setq header-line-format
                        (propertize
                         (format "%s" (or (e2wm-slack::get-room-identifier team room) "*Unknown*"))
                         'face
                         (if (eq e2wm-slack::active-slack-buffer buf)
                             'e2wm-slack:slack-active-buffer-header-face
                           'e2wm-slack:slack-inactive-buffer-header-face)))))))

(defun e2wm-slack::start-slack-if-not-yet ()
  (when (or (not slack-teams)
            (cl-loop for team in slack-teams
                     if (oref team ws-conn)
                     return nil
                     finally return t))
    (slack-start)))

(defvar e2wm-slack::current-slack-buffers nil)

(defun e2wm-slack::get-next-slack-buffers ()
  (let* ((buffers (cl-loop for b in (buffer-list)
                          if (e2wm-slack::slack-buffer-p b)
                          collect b))
         (buffers (-sort 'e2wm-slack::sort-slack-buffer buffers)))
    (setq e2wm-slack::current-slack-buffers buffers)))

(defun e2wm-slack::sort-slack-buffer (a b)
  (cond ((and e2wm-slack::next-prior-slack-buffer
              (equal e2wm-slack::next-prior-slack-buffer b))
         nil)
        ((member (buffer-local-value 'slack-current-room-id b) e2wm-slack::pinned-room-ids)
         t)
        (t
         t)))


;;;;;;;;;;;;;;;;;;
;; User Command

(defmacro e2wm-slack::def-active-buffer-command (direction)
  (let ((func-sym (intern (format "e2wm-slack:active-%s-buffer-command" (symbol-name direction))))
        (func-doc (format "Set %s buffer of current active buffer to a target of slack perspective commands." (symbol-name direction)))
        (windmove-func-sym (intern (format "windmove-%s" (symbol-name direction)))))
    `(defun ,func-sym ()
       ,func-doc
       (interactive)
       (e2wm-slack--log 'trace ,(format "start active %s buffer command." (symbol-name direction)))
       (e2wm:not-minibuffer
        (save-window-excursion
          (select-window (get-buffer-window e2wm-slack::active-slack-buffer))
          (funcall ',windmove-func-sym)
          (setq e2wm-slack::active-slack-buffer (current-buffer)))
        (e2wm-slack::input-update-target-room)
        (e2wm-slack::update-slack-buffer-header e2wm-slack::current-slack-buffers)))))

(e2wm-slack::def-active-buffer-command up)
(e2wm-slack::def-active-buffer-command down)
(e2wm-slack::def-active-buffer-command left)
(e2wm-slack::def-active-buffer-command right)

(defun e2wm-slack:load-history-if-necessary ()
  (interactive)
  (e2wm-slack--log 'trace "start load history if necessary.")
  (when (eq (window-start) (point-min))
    (slack-room-load-prev-messages)))

(defun e2wm-slack:enlarge-active-room-window ()
  (interactive)
  (e2wm-slack::relayout-windows (list e2wm-slack::active-slack-buffer)))

(defun e2wm-slack:enlarge-input-window ()
  (interactive)
  (with-selected-window (wlf:get-window (e2wm:pst-get-wm) 'input)
    (enlarge-window 1)))

(defun e2wm-slack:filter-read-room ()
  (interactive)
  (with-selected-window (wlf:get-window (e2wm:pst-get-wm) 'rooms)
    (direx-slack-room:filter-read-room)))

(defun e2wm-slack:show-filtered-room ()
  (interactive)
  (with-selected-window (wlf:get-window (e2wm:pst-get-wm) 'rooms)
    (direx-slack-room:clear-filter)))

(defun e2wm-slack:select-next-room (&optional arg)
  (interactive)
  (with-selected-window (wlf:get-window (e2wm:pst-get-wm) 'rooms)
    (when (e2wm-slack::move-succeed-p (direx:next-item arg))
      (direx:find-item))))

(defun e2wm-slack:select-previous-room ()
  (interactive)
  (e2wm-slack:select-next-room -1))

(defun e2wm-slack:select-next-unread-room (&optional arg)
  (interactive)
  (with-selected-window (wlf:get-window (e2wm:pst-get-wm) 'rooms)
    (when (e2wm-slack::move-succeed-p (direx-slack-room:next-unread-room arg))
      (direx:find-item))))

(defun e2wm-slack:select-previous-unread-room ()
  (interactive)
  (e2wm-slack:select-next-unread-room -1))

(defvar e2wm-slack:dp-minor-mode-map
  (e2wm:define-keymap
   '(("prefix s" . slack-select-rooms)
     ("prefix h" . e2wm-slack:active-left-buffer-command)
     ("prefix j" . e2wm-slack:active-down-buffer-command)
     ("prefix k" . e2wm-slack:active-up-buffer-command)
     ("prefix l" . e2wm-slack:active-right-buffer-command)
     ("prefix H" . e2wm-slack:load-history-if-necessary)
     ("prefix e" . e2wm-slack:enlarge-active-room-window)
     ("prefix i" . e2wm-slack:enlarge-input-window)
     ("prefix F" . e2wm-slack:filter-read-room)
     ("prefix A" . e2wm-slack:show-filtered-room)
     ("C-,"      . e2wm-slack:select-next-room)
     ("C-."      . e2wm-slack:select-previous-room)
     ("M-,"      . e2wm-slack:select-next-unread-room)
     ("M-."      . e2wm-slack:select-previous-unread-room))
   e2wm:prefix-key))

;;;###autoload
(defun e2wm-slack:dp ()
  "Start perspective for work in slack."
  (interactive)
  (e2wm:pst-change 'slack))


;;;;;;;;;;;;;;;;;;
;; Rooms Plugin

(e2wm:plugin-register 'slack-rooms
                      "Slack Rooms"
                      'e2wm-slack:def-plugin-rooms)

(defun e2wm-slack:def-plugin-rooms (frame wm winfo)
  (let* ((buf (direx-slack-room:ensure-buffer)))
    (wlf:set-buffer wm (wlf:window-name winfo) buf)))


;;;;;;;;;;;;;;;;;;
;; Input Plugin

(e2wm:plugin-register 'slack-input
                      "Slack Input"
                      'e2wm-slack:def-plugin-input)

(defun e2wm-slack:def-plugin-input (frame wm winfo)
  (e2wm:message "#Plugin Slack Input")
  (let ((buf (or (e2wm-slack::get-buffer-if-live slack-message-write-buffer-name)
                 (e2wm-slack::get-buffer-if-live slack-message-edit-buffer-name)
                 (generate-new-buffer slack-message-write-buffer-name))))
    (wlf:set-buffer wm (wlf:window-name winfo) buf)
    (e2wm-slack::input-update-target-room buf)))

(defvar e2wm-slack:input-mode-map
  (e2wm:define-keymap
   '(("<C-return>" . e2wm-slack:input-send-message)
     ("C-c C-c"    . e2wm-slack:input-send-message)
     ("@"          . slack-message-embed-mention)
     ("#"          . slack-message-embed-channel)
     (":"          . emoji-cheat-sheet-plus-insert)
     ("C-:"        . emoji-cheat-sheet-plus-buffer)))
  "")

(defun e2wm-slack:input-send-message ()
  (interactive)
  (e2wm-slack--log 'trace "start input send message.")
  (slack-message-send-from-buffer)
  (with-current-buffer e2wm-slack::active-slack-buffer
    (slack-message-write-another-buffer)))

(defun e2wm-slack::input-setup-buffer (&optional buf)
  (e2wm-slack--log 'trace "start input setup buffer. buf[%s]" buf)
  (with-current-buffer (or buf
                           (wlf:get-buffer (e2wm:pst-get-wm) 'input))
    (buffer-enable-undo)
    (when (featurep 'emoji-cheat-sheet-plus)
      (emoji-cheat-sheet-plus-display-mode))
    (use-local-map e2wm-slack:input-mode-map)
    (e2wm-slack::input-update-target-room buf)))

(defun e2wm-slack::input-update-target-room (&optional buf)
  (e2wm-slack--log 'trace "start input update target room. buf[%s]" buf)
  (destructuring-bind (team room) (e2wm-slack::get-team-and-room-of-buffer e2wm-slack::active-slack-buffer)
    (when (and team room)
      (with-current-buffer (or buf
                               (wlf:get-buffer (e2wm:pst-get-wm) 'input))
        (slack-buffer-set-current-room-id room)
        (slack-buffer-set-current-team-id team)
        (setq header-line-format
              (format "Message to %s"
                      (or (e2wm-slack::get-room-identifier team room) "*not selected*")))))))


;;;;;;;;;;;;;;;;;;;;;;;;
;; Third Party Plugin

(when (featurep 'emoji-cheat-sheet-plus)
  
  (defun e2wm-slack:emoji-cheat-sheet-plus-insert-from-buffer ()
    (interactive)
    (let ((code (emoji-cheat-sheet-plus--code-under-point)))
      (when code
        (with-current-buffer (wlf:get-buffer (e2wm:pst-get-wm) 'input)
          (insert code)))))

  (defun e2wm-slack:emoji-cheat-sheet-plus-setup-buffer ()
    (when (e2wm-slack:managed-p)
      (local-set-key (kbd "q")   'delete-window)
      (local-set-key (kbd "RET") 'e2wm-slack:emoji-cheat-sheet-plus-insert-from-buffer)))

  (add-hook 'emoji-cheat-sheet-plus-buffer-mode-hook 'e2wm-slack:emoji-cheat-sheet-plus-setup-buffer t)
  
  )


;;;###autoload
(defmacro e2wm-slack:history-autoloadable-commandize (command)
  `(defadvice ,command (after e2wm-slack:history-autoload disable)
     (e2wm-slack:load-history-if-necessary)))



(provide 'e2wm-slack)
;;; e2wm-slack.el ends here
