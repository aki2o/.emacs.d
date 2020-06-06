
(eval-when-compile (require 'cl))
(require 'auto-complete)
(require 'request)
(require 'json)
(require 'projectile)
(require 'log4e)
(require 'yaxception)

(defgroup rsense nil
  "Auto completion for Ruby."
  :group 'completion
  :prefix "rsense-")

(defcustom rsense-popup-help-key nil
  "Keystroke for popup help about something at point."
  :type 'string
  :group 'rsense)

(defcustom rsense-display-help-buffer-key nil
  "Keystroke for display help buffer about something at point."
  :type 'string
  :group 'rsense)

(defcustom rsense-jump-to-definition-key nil
  "Keystroke for jump to method definition at point."
  :type 'string
  :group 'rsense)

(defcustom rsense-enable-modes '(ruby-mode)
  "Major modes rsense is enabled on."
  :type '(repeat symbol)
  :group 'rsense)


(log4e:deflogger "rsense" "%t [%l] %m" "%H:%M:%S" '((fatal . "fatal")
                                                    (error . "error")
                                                    (warn  . "warn")
                                                    (info  . "info")
                                                    (debug . "debug")
                                                    (trace . "trace")))
(rsense--log-set-level 'trace)


;; (defun rsense-start-command ()
;;   (concat rsense-executable " start --path " default-directory))

;; (defun rsense-start ()
;;   (call-process-shell-command (rsense-start-command)))


(defun* rsense--show-message (msg &rest args)
  (apply 'message (concat "[RSENSE] " msg) args)
  nil)


(defun rsense--get-server-response ()
  (interactive)
  (let* ((param (rsense--request-parameter)))
    (message "PARAMETER...\n%s" (json-encode param))
    (rsense--trace "start to get server response\n%s" param)
    (request-response-data
     (request
     "http://localhost:47367"
     :type "POST"
     :data (json-encode param)
     :headers '(("Content-Type" . "application/json"))
     :parser 'json-read
     :success (cl-function
               (lambda (&key data &allow-other-keys)
                 (message "Finished!!!")
                 (assoc-default 'completions data)))
     ;; :sync t
     ))))

(defun rsense--request-parameter ()
  (let ((req-colidx (- (1+ (current-column))
                       (length ac-prefix))))
    (message "req column : %s" req-colidx)
    `((command . code_completion)
      (project . ,(projectile-project-root))
      (file . ,buffer-file-name)
      (code . ,(buffer-string))
      (location . ((row . ,(line-number-at-pos))
                   (column . ,req-colidx))))))


(defun rsense--get-ac-candidates ()
  (let* ((response (rsense--get-server-response))
         (completions (assoc-default 'completions response)))
    (mapcar (lambda (el) (cdr (assoc 'name el))) completions)))


(defvar ac-source-rsense-member
  '((candidates . rsense--get-ac-candidates)
    (prefix . "\\.\\([a-zA-Z0-9_]*\\)")
    (symbol . "m")
    ;; (document . rsense--get-ac-document)
    (requires . 0)
    (cache)
    (limit . 200)))




(defun rsense-setup-current-buffer ()
  "Do setup for using rsense in current buffer."
  (interactive)
  (yaxception:$
    (yaxception:try
        ;; Key binding
      (when (and (stringp rsense-popup-help-key)
                 (not (string= rsense-popup-help-key "")))
        (local-set-key (read-kbd-macro rsense-popup-help-key) 'rsense-popup-help))
      (when (and (stringp rsense-display-help-buffer-key)
                 (not (string= rsense-display-help-buffer-key "")))
        (local-set-key (read-kbd-macro rsense-display-help-buffer-key) 'rsense-display-help-buffer))
      (when (and (stringp rsense-jump-to-definition-key)
                 (not (string= rsense-jump-to-definition-key "")))
        (local-set-key (read-kbd-macro rsense-jump-to-definition-key) 'rsense-jump-to-definition))
      ;; For auto-complete
      ;; (add-to-list 'ac-sources 'ac-source-rsense-member)
      (setq ac-sources '(ac-source-rsense-member))
      (auto-complete-mode t)
      ;; ;; For eldoc
      ;; (set (make-local-variable 'eldoc-documentation-function) 'rsense--echo-method-usage)
      ;; (turn-on-eldoc-mode)
      (rsense--info "finished setup for %s" (current-buffer)))
    (yaxception:catch 'error e
      (rsense--show-message "Failed setup : %s" (yaxception:get-text e))
      (rsense--error "failed setup : %s\n%s"
                      (yaxception:get-text e)
                      (yaxception:get-stack-trace-string e)))))
  
(defun rsense-config-default ()
  "Do setting recommemded configuration."
  (loop for mode in rsense-enable-modes
        for hook = (intern-soft (concat (symbol-name mode) "-hook"))
        do (add-to-list 'ac-modes mode)
        if (and hook
                (symbolp hook))
        do (add-hook hook 'rsense-setup-current-buffer t)))


(provide 'rsense)
