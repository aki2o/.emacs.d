(require 'cl-lib)
(require 'direx)
(require 'slack)
(require 'log4e)


(defgroup direx-slack-room nil
  "Directory Explorer for Slack Room."
  :group 'direx
  :prefix "direx-slack-room:")

(defface direx-slack-room:team-face
  '((t (:inherit dired-directory)))
  "Face for team."
  :group 'direx-slack-room)

(defface direx-slack-room:room-face
  nil
  "Face for room."
  :group 'direx-slack-room)

(defface direx-slack-room:unread-room-face
  '((t (:bold t)))
  "Face for room has unread messages."
  :group 'direx-slack-room)


(log4e:deflogger "direx-slack-room" "%t [%l] %m" "%H:%M:%S" '((fatal . "fatal")
                                                              (error . "error")
                                                              (warn  . "warn")
                                                              (info  . "info")
                                                              (debug . "debug")
                                                              (trace . "trace")))

(direx-slack-room--log-set-level 'trace)


(defclass direx-slack-room:element (direx:tree)
  ((entity :initarg :entity :accessor direx-slack-room:element-entity)))

(defclass direx-slack-room:team (direx-slack-room:element direx:node) ())

(defclass direx-slack-room:room (direx-slack-room:element direx:leaf) ())

(defclass direx-slack-room:team-item (direx:item) ())

(defclass direx-slack-room:room-item (direx:item) ())

(defmethod direx:tree-equals ((x direx-slack-room:element) y)
  (eql (oref (direx-slack-room:element-entity x) id)
       (oref (direx-slack-room:element-entity y) id)))


(defsubst direx-slack-room--room-has-unread-p (room)
  (let ((unreads (when (not (slack-team-p room))
                   (oref room unread-count-display))))
    (when (and unreads (> unreads 0))
      unreads)))

(defsubst direx-slack-room--make-room-display (room)
  (format "%s%s"
          (slack-room-name room)
          (direx:aif (direx-slack-room--room-has-unread-p room) (format " (%d)" it) "")))

(defmethod direx:node-children ((team direx-slack-room:team))
  (direx-slack-room--trace "start node children : %s" (direx:tree-name team))
  (cl-loop for room in (with-slots (groups ims channels) (direx-slack-room:element-entity team)
                         (append channels groups ims))
           if (and (slack-room-member-p room)
                   (not (slack-room-archived-p room))
                   (slack-room-open-p room))
           collect (direx-slack-room--make-room room)))

(defmethod direx:make-item ((team direx-slack-room:team) parent)
  (make-instance 'direx-slack-room:team-item
                 :tree team
                 :parent parent
                 :face 'direx-slack-room:team-face))

(defsubst direx-slack-room--get-room-face (room)
  (direx:aif (direx-slack-room--room-has-unread-p room)
      'direx-slack-room:unread-room-face
    'direx-slack-room:room-face))

(defmethod direx:make-item ((room direx-slack-room:room) parent)
  (make-instance 'direx-slack-room:room-item
                 :tree room
                 :parent parent
                 :face (direx-slack-room--get-room-face (direx-slack-room:element-entity room))))

(defun direx-slack-room--make-team (team)
  (make-instance 'direx-slack-room:team
                 :name (slack-team-name team)
                 :entity team))

(defun direx-slack-room--make-room (room)
  (make-instance 'direx-slack-room:room
                 :name (direx-slack-room--make-room-display room)
                 :entity room))

(defmethod direx:generic-find-item ((item direx-slack-room:team-item) not-this-window)
  (direx:toggle-item item))

(defmethod direx:generic-view-item ((item direx-slack-room:team-item) not-this-window)
  (direx:toggle-item item))

(defmethod direx:generic-display-item ((item direx-slack-room:team-item) not-this-window)
  (direx:toggle-item item))

(defun direx-slack-room:open-room (room-item)
  (let* ((tree (direx:item-tree room-item))
         (room (direx-slack-room:element-entity tree)))
    ;; do async to avoid freeze by maybe slack.el trouble
    (run-with-idle-timer 0.2 nil
                         '(lambda (room)
                            (slack-room-make-buffer-with-room
                             room (slack-team-find (oref room team-id))
                             :update nil))
                         room)))

(defmethod direx:generic-find-item ((item direx-slack-room:room-item) not-this-window)
  (direx-slack-room:open-room item))

(defmethod direx:generic-view-item ((item direx-slack-room:room-item) not-this-window)
  (direx-slack-room:open-room item))

(defmethod direx:generic-display-item ((item direx-slack-room:room-item) not-this-window)
  (direx-slack-room:open-room item))

(defvar direx-slack-room--buffer-name "*Direx Slack Room*")

(defvar direx-slack-room:mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map direx:direx-mode-map)
    (define-key map (kbd "N") 'direx-slack-room:next-unread-room)
    (define-key map (kbd "P") 'direx-slack-room:previous-unread-room)
    (define-key map (kbd "F") 'direx-slack-room:filter-read-room)
    (define-key map (kbd "C") 'direx-slack-room:clear-filter)
    map))

(define-derived-mode direx-slack-room:mode direx:direx-mode "Direx Slack Room")

(defun direx-slack-room--get-buffer ()
  (or (direx:awhen (get-buffer direx-slack-room--buffer-name)
        (when (buffer-live-p it) it))
      (with-current-buffer (get-buffer-create direx-slack-room--buffer-name)
        (direx-slack-room:mode)
        (setq-local revert-buffer-function 'direx:revert-buffer)
        (current-buffer))))

(defmethod direx:make-buffer ((root direx-slack-room:team))
  (direx-slack-room--trace "start make buffer : %s" (direx:tree-name root))
  (direx-slack-room--get-buffer))

(defun direx-slack-room:update-room (room)
  (direx-slack-room--log 'trace "start update room. room[%s]" (ignore-errors (slack-room-name room)))
  (with-current-buffer (direx-slack-room--get-buffer)
    (save-excursion
      (widen)
      (cl-loop with room-element = (direx-slack-room--make-room room)
               initially (goto-char (point-min))
               while (not (eobp))
               for item = (direx:item-at-point!)
               for curr-element = (direx:item-tree item)
               if (direx:tree-equals curr-element room-element)
               return (let ((parent (direx:item-parent item)))
                        (direx:item-delete item)
                        (direx:item-insert (direx:make-item room-element parent))
                        t)
               do (forward-line 1)
               finally return nil))))


(defun direx-slack-room:ensure-buffer ()
  (cl-loop for team in (slack-team-connected-list)
           for buf = (direx:ensure-buffer-for-root (direx-slack-room--make-team team))
           finally return buf))

(defun direx-slack-room:next-unread-room (&optional arg)
  (interactive)
  (cl-loop with direction = (if arg -1 1)
           while (not (eobp))
           for item = (direx:item-at-point)
           for element = (when item (direx:item-tree item))
           for entity = (when element (direx-slack-room:element-entity element))
           if (direx-slack-room--room-has-unread-p entity)
           return (direx:move-to-item-name-part item)
           do (forward-line direction)
           finally return nil))

(defun direx-slack-room:previous-unread-room ()
  (interactive)
  (direx-slack-room:next-unread-room 1))

(defun direx-slack-room:filter-read-room ()
  (interactive)
  (let* ((curr-item (direx:item-at-point))
         (curr-root (when curr-item (direx:item-parent curr-item))))
    (save-excursion
      (cl-loop initially (goto-char (point-min))
               while (not (eobp))
               for item = (direx:item-at-point)
               for element = (when item (direx:item-tree item))
               for entity = (when element (direx-slack-room:element-entity element))
               if item
               do (cond ((direx-slack-room--room-has-unread-p entity)
                         (direx:item-show item))
                        ((not (slack-team-p entity))
                         (direx:item-hide item)))
               do (forward-line 1)))
    (when curr-root
      (direx:goto-item-for-tree (direx:item-tree curr-root)))))

(defun direx-slack-room:clear-filter ()
  (interactive)
  (save-excursion
    (cl-loop initially (goto-char (point-min))
             while (not (eobp))
             for item = (direx:item-at-point)
             for element = (when item (direx:item-tree item))
             for entity = (when element (direx-slack-room:element-entity element))
             if (and (slack-room-p entity)
                     (not (direx:item-visible-p item)))
             do (direx:item-show item)
             do (forward-line 1))))


;;;###autoload
(defun direx-slack-room:open-buffer ()
  (interactive)
  (switch-to-buffer-other-window (direx-slack-room:ensure-buffer)))

;;;###autoload
(defun direx-slack-room:set-update-room-automatically (t_or_nil)
  (if t_or_nil
      (ad-enable-advice 'slack-buffer-update 'after 'direx-slack-room:update-node)
    (ad-disable-advice 'slack-buffer-update 'after 'direx-slack-room:update-node))
  (ad-activate 'slack-buffer-update))


(defadvice slack-buffer-update (after direx-slack-room:update-node disable)
  (direx-slack-room:update-room (ad-get-arg 0)))


(provide 'direx-slack-room)
;;; direx-slack-room.el ends here
