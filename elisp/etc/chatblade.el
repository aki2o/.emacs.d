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
  "Alist of template name and their function to be called on selecting the template.
If not set, it will be `chatblade-open-interactive'."
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

(defcustom chatblade-default-arguments '("--raw" "--no-format")
  "Default arguments to pass to chatblade."
  :type '(repeat string)
  :group 'chatblade)

(defcustom chatblade-input-fold-threshold 80
  "Threshold to fold input."
  :type 'integer
  :group 'chatblade)

(defcustom chatblade-send-input-by-newline-p t
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

(defvar chatblade--query nil)
(defvar chatblade--session nil)
(defvar chatblade--prompt-name nil)
(defvar chatblade--template-name nil)

(defun chatblade--resolve-prompt-by (mode)
  (let* ((prompt (cl-loop while mode
                          for prompt = (assoc-default mode chatblade-prompt-alist)
                          if prompt return prompt
                          else do (setq mode (get mode 'derived-mode-parent))))
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
    (let* ((session (chatblade--new-session template-name)))
      (insert query "\n")
      (font-lock-append-text-property (point-min) (point) 'font-lock-face 'font-lock-comment-face)
      (set-marker comint-last-input-start (point-min))
      (set-marker comint-last-input-end (point))
      (apply #'make-comint-in-buffer
             "chatblade"
             (current-buffer)
             chatblade-executable-path
             nil
             (chatblade--make-arguments query chatblade--prompt-name session "--interactive" "--only"))
      (setq-local chatblade--query query)
      (setq-local chatblade--session session)
      (setq-local chatblade--prompt-name chatblade--prompt-name)
      (current-buffer))))

(defun chatblade--make-arguments (query prompt session &rest args)
  (let* ((prompt (when prompt (list "--prompt-file" prompt)))
         (session (when session (list "--session" session))))
    (when query (setq args `(,@args ,query)))
    `(,@prompt ,@session ,@chatblade-default-arguments ,@args)))

(defun chatblade--fold-input (beg end)
  (let ((ov (make-overlay beg end)))
    (overlay-put ov 'creator 'chatblade)
    (overlay-put ov 'isearch-open-invisible 'chatblade--isearch-open-invisible)
    (overlay-put ov 'isearch-open-invisible-temporary 'chatblade--isearch-open-invisible-temporary)
    (chatblade--input-fold-close ov)
    ov))

(defun chatblade--input-fold-close (ov)
  (overlay-put ov 'invisible t)
  (overlay-put ov 'before-string (propertize "..." 'face 'font-lock-comment-face)))

(defun chatblade--input-fold-open (ov)
  (overlay-put ov 'invisible nil)
  (overlay-put ov 'before-string nil))

(defun chatblade--input-fold-close-p (ov)
  (overlay-get ov 'invisible))

(defun chatblade--input-fold-at (pt)
  (let ((pt (save-excursion
              (goto-char pt)
              (- (pos-eol) 1))))
    (-find (lambda (ov)
             (eq (overlay-get ov 'creator) 'chatblade))
           (overlays-at pt))))

(defun chatblade--isearch-open-invisible (ov)
  (delete-overlay ov))

(defun chatblade--isearch-open-invisible-temporary (ov hide-p)
  (overlay-put ov 'invisible (if hide-p 'yaol nil)))

(defun chatblade--make-up-input-region (_string)
  (let* ((beg (marker-position comint-last-input-start))
         (end (marker-position comint-last-input-end))
         (end (if (eq (char-before end) ?\n) (- end 1) end))
         (inhibit-read-only t))
    (font-lock-append-text-property beg end 'font-lock-face 'font-lock-comment-face)
    (save-excursion
      (goto-char beg)
      (forward-char chatblade-input-fold-threshold)
      (when (and (> end (point))
                 (not (chatblade--input-fold-at (point))))
        (chatblade--fold-input (point) end)))))

(defun chatblade--make-up-output-region (_string)
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-max))
      (forward-line -1)
      (when (re-search-forward chatblade-interactive-prompt-regexp (point-max) t)
        (font-lock-append-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'comint-highlight-prompt)
        (goto-char (match-beginning 0))
        (goto-char (pos-bol))
        (insert "\n")))))

(defun chatblade--filter-input (string)
  (if (string= string "quit")
      string
    (let* ((s (s-trim string))
           (s (replace-regexp-in-string "\n\\'" "" s))
           (s (replace-regexp-in-string "\\\\n" "\\\\ n" s))
           (s (replace-regexp-in-string "\n" "\\\\n" s)))
      (when (> (length s) 0)
        (concat "\"" s "\"")))))

(defun chatblade--input-sender (proc input)
  (let ((s (chatblade--filter-input input)))
    (when s (comint-simple-send proc s))))

(defun chatblade--resolve-fenced-mode ()
  (when (re-search-forward (rx (and "```")) (pos-eol) t)
    (let* ((lang (s-trim (buffer-substring-no-properties (point) (pos-eol))))
           (mode (when (> (length lang) 0) (intern-soft (format "%s-mode" lang)))))
      (or mode
          (assoc-default lang chatblade-fenced-code-mode-alist)
          'fundamental-mode))))

;;;###autoload
(defun chatblade-open-interactive (query)
  (switch-to-buffer-other-window (chatblade--new-buffer query)))

;;;###autoload
(cl-defun chatblade-request (query &key prompt session (omit-query t))
  (let* ((prompt (if prompt prompt (chatblade--resolve-prompt-by major-mode)))
         (args (if omit-query '("--only") '()))
         (args (apply 'chatblade--make-arguments query prompt session args)))
    (message "DEBUG chatblade-request : %s" args)
    (shell-command-to-string (mapconcat 'shell-quote-argument `(,chatblade-executable-path ,@args) " "))))

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

(defvar-keymap chatblade-mode-map
  :doc "Mode map used for `chatblade-mode'.
Based on `comint-mode-map'."
  :parent comint-mode-map
  "C-m"        #'chatblade-maybe-send-input
  "S-<return>" #'chatblade-maybe-insert-newline
  "<tab>"      #'chatblade-toggle-current-input-fold
  "C-c C-c"    #'comint-send-input
  "C-c C-r"    #'chatblade-resume-current
  "C-c C-d"    #'chatblade-describe-current-session)

(define-derived-mode chatblade-mode comint-mode "Chat"
  "Major mode to communicate with chatblade.
\\{chatblade-mode-map}"
  (setq-local comint-prompt-read-only nil)
  (setq-local comint-use-prompt-regexp nil)
  (setq-local comint-prompt-regexp chatblade-interactive-prompt-regexp)
  (setq-local comint-input-sender 'chatblade--input-sender)
  (add-hook 'comint-output-filter-functions #'chatblade--make-up-input-region nil t)
  (add-hook 'comint-output-filter-functions #'chatblade--make-up-output-region nil t)
  (poly-chatblade-mode))

(defun chatblade-maybe-send-input ()
  (interactive)
  (if chatblade-send-input-by-newline-p (comint-send-input) (self-insert-command)))

(defun chatblade-maybe-insert-newline ()
  (interactive)
  (if chatblade-send-input-by-newline-p (insert "\n") (comint-send-input)))

(defun chatblade-toggle-current-input-fold ()
  (interactive)
  (let ((ov (chatblade--input-fold-at (point))))
    (when ov
      (if (chatblade--input-fold-close-p ov)
          (chatblade--input-fold-open ov)
        (chatblade--input-fold-close ov)))))

(defun chatblade-resume-current ()
  (interactive)
  (chatblade-resume (current-buffer)))

(defun chatblade-describe-current-session ()
  (interactive)
  (chatblade-describe-session (current-buffer)))

;;;###autoload
(defun chatblade-describe-session (buffer)
  (interactive (list (chatblade--select-buffer)))
  (let ((session (buffer-local-value 'chatblade--session buffer))
        (prompt (buffer-local-value 'chatblade--prompt-name buffer)))
    (when (not session)
      (error "Not found chatblade--session in %s" buffer))
    (pop-to-buffer
     (with-current-buffer (generate-new-buffer "*chatblade:session*")
       (insert (chatblade-request "--token" :prompt prompt :session session :omit-query nil))
       (current-buffer)))))

;;;###autoload
(defun chatblade-resume (buffer)
  (interactive (list (chatblade--select-buffer)))
  (if (comint-check-proc buffer)
      (chatblade-switch-to-buffer buffer)
    (with-current-buffer buffer
      (goto-char (point-max))
      (let* ((session (buffer-local-value 'chatblade--session (current-buffer)))
             (prompt (buffer-local-value 'chatblade--prompt-name (current-buffer)))
             (proc (apply 'start-process
                          "chatblade"
                          (current-buffer)
                          chatblade-executable-path
                          (chatblade--make-arguments nil prompt session "--interactive" "--only"))))
        (insert "Resumed!\n\n")
        (set-marker (process-mark proc) (point))))
    (when (not (eql buffer (current-buffer)))
      (switch-to-buffer-other-window buffer))))

;;;###autoload
(defun chatblade-start (prompt-name template-name)
  (interactive (list (chatblade--resolve-prompt-by major-mode)
                     (chatblade--select-query-template)))
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
         (chatblade--prompt-name prompt-name)
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
