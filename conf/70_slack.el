(use-package slack
  :defer t
  :commands (slack-start)
  :init
  (use-package oauth2 :defer t)
  (use-package circe :defer t)
  (use-package websocket :defer t)
  
  (setq slack-buffer-emojify t)
  (setq slack-prefer-current-team t)

  ;; (defvar slack-current-room-id nil)
  ;; (defvar slack-current-team-id nil)
  ;; (defvar slack-message-edit-buffer-type nil)
  ;; (make-variable-buffer-local 'slack-current-room-id)
  ;; (make-variable-buffer-local 'slack-current-team-id)
  ;; (make-variable-buffer-local 'slack-message-edit-buffer-type)
  
  :config
  (slack-register-team
   :name "mf"
   :default t
   :client-id (~auth-source-get-property 'id :app "slack.mf")
   :client-secret (~auth-source-get-property 'secret :app "slack.mf")
   :token (~auth-source-get-property 'token :app "slack.mf")
   :subscribed-channels '(pa_team my_number))

  ;; p-r

  ;; cache auth

  (defvar slack-oauth2-cache-authorize t)
  
  (defun slack-oauth2-auth (team)
    (with-slots (client-id client-secret) team
      (if slack-oauth2-cache-authorize
          (oauth2-auth-and-store
           slack-oauth2-authorize
           slack-oauth2-access
           "client"
           client-id
           client-secret
           slack-redirect-url)
        (oauth2-auth
         slack-oauth2-authorize
         slack-oauth2-access
         client-id
         client-secret
         "client"
         nil
         slack-redirect-url))))

  ;; select last line at first
  
  (defun slack-get-buffer-create (room)
    (let* ((buf-name (slack-room-buffer-name room))
           (buffer (get-buffer buf-name)))
      (unless buffer
        (setq buffer (generate-new-buffer buf-name))
        (with-current-buffer buffer
          (slack-mode)
          (slack-buffer-insert-previous-link room)
          (add-hook 'kill-buffer-hook 'slack-reset-room-last-read nil t)
          (add-hook 'lui-pre-output-hook 'slack-buffer-add-last-ts-property nil t)
          (add-hook 'lui-post-output-hook 'slack-buffer-add-ts-property nil t)
          (goto-char (point-max))))
      buffer))

  (defun slack-get-info-buffer-create (room)
    (let* ((buf-name (slack-room-buffer-name room))
           (buffer (get-buffer buf-name)))
      (unless buffer
        (setq buffer (generate-new-buffer buf-name))
        (with-current-buffer buffer
          (slack-info-mode)
          (slack-buffer-insert-previous-link room)
          (add-hook 'kill-buffer-hook 'slack-reset-room-last-read nil t)
          (add-hook 'lui-pre-output-hook 'slack-buffer-add-last-ts-property nil t)
          (add-hook 'lui-post-output-hook 'slack-buffer-add-ts-property nil t)
          (goto-char (point-max))))
      buffer))

  ;; use emoji-cheat-sheet-plus

  (require 'emoji-cheat-sheet-plus nil t)
  
  (defun slack-message-reactions-input-using-emoji-cheat-sheet-plus ()
    (cl-loop for reaction in (split-string (with-temp-buffer
                                             (emoji-cheat-sheet-plus-insert)
                                             (buffer-string))
                                           "::")
             for reaction = (replace-regexp-in-string "\\`:" "" reaction)
             for reaction = (replace-regexp-in-string ":\\'" "" reaction)
             collect reaction))

  (defun slack-message-add-reaction ()
    (interactive)
    (let* ((word (thing-at-point 'word))
           (ts (get-text-property 0 'ts word))
           (team (slack-team-find slack-current-team-id))
           (room (slack-room-find slack-current-room-id team))
           (reactions (if (featurep 'emoji-cheat-sheet-plus)
                          (slack-message-reactions-input-using-emoji-cheat-sheet-plus)
                        (list (slack-message-reaction-input)))))
      (dolist (reaction reactions)
        (slack-message-reaction-add reaction ts room team)))))


(use-package e2wm-slack
  :straight (:type built-in)
  :after (e2wm)
  :defer t
  :config
  (e2wm-slack--log-enable-logging)
  
  (e2wm-slack:history-autoloadable-commandize scroll-down)
  ;; (e2wm-slack:history-autoloadable-commandize inertias-down)
  )


(use-package direx-slack-room
  :straight (:type built-in)
  :after (slack)
  :defer t
  :config
  (direx-slack-room--log-enable-logging)
  (direx-slack-room:set-update-room-automatically t)

  (when (fboundp 'pophint:defsource)
    ;; slack.elがフリーズするのでrun-with-idle-timerを使うようにしておく
    (pophint:defsource :name "direx-node"
                       :description "Node on DireX."
                       :source '((shown . "Node")
                                 (regexp . pophint-direx:node-regexp)
                                 (requires . 1)
                                 (highlight . nil)
                                 (dedicated . (e2wm))
                                 (activebufferp . (lambda (b)
                                                    (pophint--maybe-kind-mode-buffer-p b 'direx:direx-mode)))
                                 (action . (lambda (hint)
                                             (if (not (ignore-errors (e2wm:managed-p)))
                                                 (funcall pophint--default-action hint)
                                               (with-selected-window (pophint:hint-window hint)
                                                 (goto-char (pophint:hint-startpt hint))
                                                 (run-with-idle-timer
                                                  0.2 nil
                                                  '(lambda (item) (direx:find-item-other-window item))
                                                  (direx:item-at-point!))))))))))
