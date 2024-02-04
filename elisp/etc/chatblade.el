(require 'comint)
(require 'rx)
(require 's)


(defcustom chatblade-executable-path "chatblade"
  "Path to the chatblade executable.")

(defcustom chatblade-config-directory (expand-file-name "~/.config/chatblade")
  "Path to the chatblade configuration file.")

(defcustom chatblade-prompt-alist '()
  "Alist of major mode and their corresponding prompt file name.")

(defcustom chatblade-query-template-alist '()
  "Alist of the name and their template string of query to send.")

(defcustom chatblade-start-function-alist '()
  "Alist of template name and their function to be called on selecting the template.")

(defcustom chatblade-prompt-template-function nil
  "Function to make a template of prompt.")

(defcustom chatblade-find-prompt-function 'chatblade--find-prompt
  "Function to find the prompt for the current buffer.")

(defcustom chatblade-default-arguments '("--raw" "--no-format" "--only")
  "Default arguments to pass to chatblade.")

(defcustom chatblade-send-input-by-return-p t
  "Whether to send input by typing <return>.
Typing S-<return> means
  - If t, sending input
  - If nil, just inputing newline")

(defcustom chatblade-session-prefix "emacs-"
  "Prefix for the chatblade session names.")

(defvar chatblade-interactive-prompt "query (type 'quit' to exit): : ")
(defvar chatblade-select-prompt-p nil)

(defvar chatblade--query nil)
(defvar chatblade--session nil)
(defvar chatblade--template-name nil)

(defun chatblade--find-prompt ()
  (let* ((mode major-mode)
         (prompt (if chatblade-select-prompt-p
                     (chatblade--select-prompt :require-match t)
                   (cdr (assoc mode chatblade-prompt-alist))))
         (file (when prompt (expand-file-name prompt chatblade-config-directory))))
    (when (and file (not (file-exists-p file)))
      (error "Not found prompt file : %s" file))
    prompt))

(defun chatblade--select-prompt (prompt &key require-match default)
  (let ((list (-filter (lambda (x) (not (s-starts-with? "." x))) (directory-files chatblade-config-directory))))
    (completing-read (or prompt "Select prompt file: ") list nil require-match nil nil default)))

(defun chatblade--select-query-template ()
  (if (= (length chatblade-query-template-alist) 0)
      (read-string "Input query template: ")
    (let* ((list (mapcar 'car chatblade-query-template-alist)))
      (completing-read "Select template or input directly: " list nil nil nil nil))))

(defun chatblade--get-sessions ()
  (let ((string (shell-command-to-string (mapconcat 'identity `(,chatblade-executable-path "--session-list") " "))))
    (split-string (s-trim string) "\n")))

(defun chatblade--new-session ()
  (let ((sessions (-filter (lambda (x) (s-starts-with? chatblade-session-prefix x)) (chatblade--get-sessions))))
    (format "%s-%d" chatblade-session-prefix (+ (length sessions) 1))))

(defun chatblade--select-buffer ()
  (let* ((buffers (-filter (lambda (x) (eq major-mode 'chatblade-mode)) buffer-list))
         (cands (mapcar (lambda (x) (buffer-local-value 'chatblade--query x)) buffers)))
    (completing-read "Select buffer: " cands nil t nil nil)))

(defun chatblade--new-buffer (query &optional template-name)
  (with-current-buffer (generate-new-buffer (if chatblade--template-name
                                                (format "*chatblade-%s*" chatblade--template-name)
                                              "*chatblade*"))
    (let* ((session (chatblade--new-session))
           (proc (apply 'start-process
                        "chatblade"
                        (current-buffer)
                        chatblade-executable-path
                        (chatblade--make-arguments query :with-prompt t "--interactive" "--session" session))))
      (set (make-local-variable 'chatblade--query) query)
      (set (make-local-variable 'chatblade--session) session)
      (insert query "\n")
      (set-marker (process-mark proc) (point))
      (chatblade-mode)
      (current-buffer))))

(defun chatblade--start (query)
  (switch-to-buffer-other-window (chatblade--new-buffer query)))

(defun chatblade--input-sender (proc string)
  (message "DEBUG! input sender : %s" string)
  ;; (comint-output-filter proc chatblade-interactive-prompt)
  (comint-simple-send proc string))

(defun chatblade--make-arguments (query &key with-prompt &rest args)
  (let* ((prompt (when with-prompt (funcall chatblade-find-prompt-function)))
         (prompt (when prompt (list "--prompt-file" prompt))))
    (when query (setq args `(,@args ,query)))
    `(,@prompt ,@chatblade-default-arguments ,@args)))

(defun chatblade-request (query &key with-prompt session)
  (let* ((args (when session `("--session" ,session)))
         (args (apply 'chatblade--make-arguments query :with-prompt with-prompt args)))
    (shell-command-to-string (mapconcat 'shell-quote-argument `(,chatblade-executable-path ,@args) " "))))

(define-derived-mode chatblade-mode comint-mode "Chat"
  "Major mode to communicate with chatblade."
  ;; :syntax-table chatblade-mode-syntax-table
  (setq-local comint-prompt-regexp (rx-to-string `(and bol ,chatblade-interactive-prompt)))
  (setq-local comint-input-sender 'chatblade--input-sender)
  (setq-local comint-input-sender-no-newline t))

(defvar-keymap chatblade-mode-map
  :parent comint-mode-map
  "C-m"        #'chatblade-maybe-send-input
  "S-<return>" #'chatblade-maybe-insert-newline
  "C-c C-c"    #'comint-send-input
  "C-c C-r"    #'chatblade-resume-current
  "C-c C-d"    #'chatblade-describe-current-session)

(defun chatblade-maybe-send-input ()
  (interactive)
  (if chatblade-send-input-by-newline-p (comint-send-input) (self-insert-command)))

(defun chatblade-maybe-insert-newline ()
  (interactive)
  (if chatblade-send-input-by-newline-p (insert "\n") (comint-send-input)))

(defun chatblade-resume-current ()
  (interactive)
  (chatblade-resume (current-buffer)))

(defun chatblade-describe-current-session ()
  (interactive)
  (chatblade-describe-session (current-buffer)))

(defun chatblade-describe-session (buffer)
  (interactive (list (chatblade--select-buffer)))
  (let ((session (buffer-local-value 'chatblade--session buffer)))
    (pop-to-buffer
     (with-temp-buffer
       (insert (chatblade-request "--token" :with-prompt nil :session session))))))

(defun chatblade-resume (buffer)
  (interactive (list (chatblade--select-buffer)))
  (with-current-buffer buffer
    (when (comint-check-proc (current-buffer))
      (error "Can't resume : process still running"))
    (goto-char (point-max))
    (let* ((session (buffer-local-value 'chatblade--session (current-buffer)))
           (proc (apply 'start-process
                        "chatblade"
                        (current-buffer)
                        chatblade-executable-path
                        (chatblade--make-arguments nil :with-prompt nil "--interactive" "--session" session))))
      (insert "Resumed!\n")
      (set-marker (process-mark proc) (point))))
  (when (not (eql buffer (current-buffer)))
    (switch-to-buffer-other-window buffer)))

(defun chatblade-start (template-name)
  (interactive (list (chatblade--select-query-template)))
  (let* ((template (or (assoc-default template-name chatblade-query-template-alist)
                       template-name))
         (performer (or (assoc-default template-name chatblade-start-function-alist)
                        'chatblade--start))
         (query (if (region-active-p)
                   (buffer-substring-no-properties (region-beginning) (region-end))
                  (buffer-substring-no-properties (point-min) (point))))
         (chatblade--template-name (if (eql template template-name) nil template-name)))
    (funcall performer (format template query))))

(defun chatblade-update-prompt-file (thing name)
  (interactive
   (let* ((default-name (replace-regexp-in-string "-mode\\'" "" (symbol-name major-mode)))
          (default-thing (mapconcat 's-capitalize (split-string default-name "-") " ")))
     (list (read-string "What do you make a prompt for? " nil nil default-thing)
           (chatblade--select-prompt "Select or input prompt file name to save: " :require-match nil :default default-name))))
  (when (not (functionp chatblade-prompt-template-function))
    (error "You need to set chatblade-prompt-template-function")))

(defun chatblade-find-prompt-file ()
  (interactive)
  (let ((default-directory (concat (directory-file-name chatblade-config-directory) "/")))
    (call-interactively 'find-file)))

(provide 'chatblade)
