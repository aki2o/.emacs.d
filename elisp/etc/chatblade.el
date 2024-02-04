(require 'comint)
(require 'rx)
(require 's)
(require 'dash)
(require 'polymode)
(require 'markdown-mode)

(defgroup chatblade nil
  "Chatblade client for Emacs."
  :group 'tools)

(defcustom chatblade-executable-path "chatblade"
  "Path to the chatblade executable."
  :type 'string
  :group 'chatblade)

(defcustom chatblade-config-directory (expand-file-name "~/.config/chatblade")
  "Path to the chatblade configuration file."
  :type 'string
  :group 'chatblade)

(defcustom chatblade-prompt-alist '()
  "Alist of major mode and their corresponding prompt file name."
  :type '(alist :key-type symbol :value-type string)
  :group 'chatblade)

(defcustom chatblade-query-template-alist '()
  "Alist of the name and their template string or function of query to send."
  :type '(alist :key-type string :value-type (choice string function))
  :group 'chatblade)

(defcustom chatblade-start-function-alist '()
  "Alist of template name and their function to be called on selecting the template."
  :type '(alist :key-type string :value-type function)
  :group 'chatblade)

(defcustom chatblade-fenced-code-mode-alist '(("lisp"  . emacs-lisp-mode)
                                              ("elisp" . emacs-lisp-mode))
  "Alist of the language name and the corresponding major mode."
  :type '(alist :key-type string :value-type symbol)
  :group 'chatblade)

(defcustom chatblade-query-filter-functions '()
  "List of functions to filter the query before sending it to chatblade."
  :type '(repeat function)
  :group 'chatblade)

(defcustom chatblade-prompt-template-function nil
  "Function to make a template of prompt."
  :type 'function
  :group 'chatblade)

(defcustom chatblade-find-prompt-function 'chatblade--find-prompt
  "Function to find the prompt for the current buffer."
  :type 'function
  :group 'chatblade)

(defcustom chatblade-default-arguments '("--raw" "--no-format")
  "Default arguments to pass to chatblade."
  :type '(repeat string)
  :group 'chatblade)

(defcustom chatblade-send-input-by-return-p t
  "Whether to send input by typing <return>.
Typing S-<return> means
  - If t, sending input
  - If nil, just inputing newline"
  :type 'boolean
  :group 'chatblade)

(defcustom chatblade-session-prefix "emacs"
  "Prefix for the chatblade session names."
  :type 'string
  :group 'chatblade)

(defvar chatblade-interactive-prompt-regexp (rx (and bol "query (type 'quit' to exit): : ")))
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

(cl-defun chatblade--select-prompt (prompt &key require-match default)
  (let ((list (-filter (lambda (x) (not (s-starts-with? "." x))) (directory-files chatblade-config-directory))))
    (completing-read (or prompt "Select prompt file: ") list nil require-match nil nil default)))

(defun chatblade--select-query-template ()
  (if (= (length chatblade-query-template-alist) 0)
      (read-string "Input query template: ")
    (let* ((list (mapcar 'car chatblade-query-template-alist)))
      (completing-read "Select template or input directly: " list nil nil nil nil))))

(defun chatblade--get-sessions ()
  (let ((string (shell-command-to-string (mapconcat 'identity `(,chatblade-executable-path "--session-list") " "))))
    (-filter (lambda (x) (s-starts-with? chatblade-session-prefix x)) (split-string (s-trim string) "\n"))))

(defun chatblade--new-session (template-name)
  (let* ((prefix (if template-name
                     (concat chatblade-session-prefix "-" template-name)
                   chatblade-session-prefix))
         (now (format-time-string "%Y%m%d-%H%M%S")))
    (format "%s-%s" prefix now)))

(defun chatblade--select-buffer ()
  (let* ((buffers (-filter (lambda (x) (eq (buffer-local-value 'major-mode x) 'chatblade-mode)) (buffer-list)))
         (cands (mapcar (lambda (x) (buffer-local-value 'chatblade--query x)) buffers))
         (answer (completing-read "Select buffer: " cands nil t nil nil)))
    (-find (lambda (x) (string= (buffer-local-value 'chatblade--query x) answer)) buffers)))

(defun chatblade--new-buffer (query &optional template-name)
  (with-current-buffer (generate-new-buffer (if chatblade--template-name
                                                (format "*chatblade %s*" chatblade--template-name)
                                              "*chatblade *"))
    (chatblade-mode)
    (let* ((session (chatblade--new-session template-name))
           (proc (apply 'start-process
                        "chatblade"
                        (current-buffer)
                        chatblade-executable-path
                        (chatblade--make-arguments query :with-prompt t "--interactive" "--only" "--session" session))))
      (insert query "\n\n")
      (set-marker (process-mark proc) (point))
      (setq-local chatblade--query query)
      (setq-local chatblade--session session)
      (current-buffer))))

(defun chatblade--make-arguments (query &key with-prompt &rest args)
  (let* ((prompt (when with-prompt (funcall chatblade-find-prompt-function)))
         (prompt (when prompt (list "--prompt-file" prompt))))
    (when query (setq args `(,@args ,query)))
    `(,@prompt ,@chatblade-default-arguments ,@args)))

(defun chatblade--setup-text-property (string)
  (message "DEBUG! called chatblade--setup-text-property : %s" string)
  (save-excursion
    (goto-char (point-max))
    (forward-line -1)
    (when (re-search-forward chatblade-interactive-prompt-regexp (point-max) t)
      (font-lock-append-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'comint-highlight-prompt)
      (add-text-properties (point-min) (point) '(read-only t)))))

;;;###autoload
(defun chatblade-open-interactive (query)
  (switch-to-buffer-other-window (chatblade--new-buffer query)))

;;;###autoload
(cl-defun chatblade-request (query &key (with-prompt t) (omit-query t) session)
  (let* ((args (when session `("--session" ,session)))
         (args (if omit-query `(,@args "--only") args))
         (args (apply 'chatblade--make-arguments query :with-prompt with-prompt args)))
    (shell-command-to-string (mapconcat 'shell-quote-argument `(,chatblade-executable-path ,@args) " "))))

(defun chatblade--resolve-fenced-mode ()
  (when (re-search-forward (rx (and "```")) (pos-eol) t)
    (let* ((lang (s-trim (buffer-substring-no-properties (point) (pos-eol))))
           (mode (when (> (length lang) 0) (intern-soft (format "%s-mode" lang)))))
      (or mode
          (assoc-default lang chatblade-fenced-code-mode-alist)
          'fundamental-mode))))

(define-hostmode poly-chatblade-hostmode :mode 'chatblade-mode)

(define-auto-innermode poly-chatblade-innermode
  :head-matcher (cons "^[ \t]*\\(```{?[[:alpha:]].*\n\\)" 1)
  :tail-matcher (cons "^[ \t]*\\(```\\)[ \t]*$" 1)
  :mode-matcher 'chatblade--resolve-fenced-mode
  :head-mode 'host
  :tail-mode 'host
  :fallback-mode 'host)

(define-polymode poly-chatblade-mode
  :hostmode 'poly-chatblade-hostmode
  :innermodes '(poly-chatblade-innermode))

(define-derived-mode chatblade-mode comint-mode "Chat"
  "Major mode to communicate with chatblade."
  (setq-local comint-prompt-read-only nil)
  (setq-local comint-use-prompt-regexp nil)
  (setq-local comint-prompt-regexp chatblade-interactive-prompt-regexp)
  (setq-local comint-input-sender-no-newline t)
  (add-hook 'comint-output-filter-functions #'chatblade--setup-text-property nil t)
  (poly-chatblade-mode))

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

;;;###autoload
(defun chatblade-describe-session (buffer)
  (interactive (list (chatblade--select-buffer)))
  (let ((session (buffer-local-value 'chatblade--session buffer)))
    (when (not session)
      (error "Not found chatblade--session in %s" buffer))
    (pop-to-buffer
     (with-current-buffer (generate-new-buffer "*chatblade:session*")
       (insert (chatblade-request "--token" :with-prompt nil :omit-query nil :session session))
       (current-buffer)))))

;;;###autoload
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
                        (chatblade--make-arguments nil :with-prompt nil "--interactive" "--only" "--session" session))))
      (insert "Resumed!\n")
      (set-marker (process-mark proc) (point))))
  (when (not (eql buffer (current-buffer)))
    (switch-to-buffer-other-window buffer)))

;;;###autoload
(defun chatblade-start (template-name)
  (interactive (list (chatblade--select-query-template)))
  (let* ((template (or (assoc-default template-name chatblade-query-template-alist)
                       template-name))
         (template (if (functionp template) (funcall template) template))
         (performer (or (assoc-default template-name chatblade-start-function-alist)
                        'chatblade-open-interactive))
         (query (if (region-active-p)
                   (buffer-substring (region-beginning) (region-end))
                  (buffer-substring (point-min) (point))))
         (query (-reduce-from (lambda (q f) (funcall f q)) query chatblade-query-filter-functions))
         (query (substring-no-properties query))
         (chatblade--template-name (if (eql template template-name) nil template-name)))
    (deactivate-mark)
    (funcall performer (format template query))))

;;;###autoload
(defun chatblade-switch-to-buffer (buffer)
  (interactive (list (chatblade--select-buffer)))
  (switch-to-buffer-other-window buffer))

;;;###autoload
(defun chatblade-clear-sessions ()
  (interactive)
  )

;;;###autoload
(defun chatblade-update-prompt-file (thing name)
  (interactive
   (let* ((default-name (replace-regexp-in-string "-mode\\'" "" (symbol-name major-mode)))
          (default-thing (mapconcat 's-capitalize (split-string default-name "-") " ")))
     (list (read-string (format "What do you make a prompt for? (%s) " default-thing) nil nil default-thing)
           (chatblade--select-prompt
            (format "Select or input prompt file name to save (%s): " default-name)
            :require-match nil
            :default default-name))))
  (when (not (functionp chatblade-prompt-template-function))
    (error "You need to set chatblade-prompt-template-function"))
  (let ((file (expand-file-name name chatblade-config-directory)))
    (with-temp-file file
      (insert (funcall chatblade-prompt-template-function thing)))
    (message "Saved to %s" file)))

;;;###autoload
(defun chatblade-find-prompt-file ()
  (interactive)
  (let ((default-directory (concat (directory-file-name chatblade-config-directory) "/")))
    (call-interactively 'find-file)))

(provide 'chatblade)
