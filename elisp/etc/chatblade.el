(require 'comint)
(require 'rx)
(require 's)
(require 'dash)
(require 'polymode)
(require 'yaml-mode)
(require 'yaml)

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

(defcustom chatblade-prompt-name-alist '()
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
                                              ("elisp" . emacs-lisp-mode)
                                              ("tsx"   . typescript-mode))
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

(defcustom chatblade-default-arguments '("--raw" "--no-format" "--stream")
  "Default arguments to pass to chatblade."
  :type '(repeat string)
  :group 'chatblade)

(defcustom chatblade-default-model nil
  "Default model to use.
For detail, see <https://github.com/npiv/chatblade?tab=readme-ov-file#help>"
  :type 'string
  :group 'chatblade)

(defcustom chatblade-default-switched-model "4t"
  "Default model to be applied on `chatblade-switch-model'."
  :type 'string
  :group 'chatblade)

(defcustom chatblade-session-outdated-dates 3
  "The number of days to consider the session outdated."
  :type 'integer
  :group 'chatblade)

(defcustom chatblade-input-fold-threshold 80
  "Threshold to fold input."
  :type 'integer
  :group 'chatblade)

(defcustom chatblade-buffer-name-truncate-width 40
  "Width to truncate buffer name."
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
(defvar chatblade--model nil)
(defvar chatblade--prompt-name nil)
(defvar chatblade--template-name nil)

(defun chatblade--resolve-prompt-name-by-session (session)
  (let* ((re (rx-to-string `(and bos ,chatblade-session-prefix "-")))
         (v (replace-regexp-in-string re "" session))
         (v (nth 0 (split-string v "-"))))
    (if (string= v "nothing") nil v)))

(defun chatblade--resolve-prompt-name-by-mode (mode)
  (let* ((prompt-name (cl-loop while mode
                               for p = (assoc-default mode chatblade-prompt-name-alist)
                               if p return p
                               else do (setq mode (get mode 'derived-mode-parent))))
         (file (when prompt-name (expand-file-name prompt-name chatblade-config-directory))))
    (when (and file (not (file-exists-p file)))
      (error "Not found prompt file : %s" file))
    prompt-name))

(cl-defun chatblade--select-prompt-name (prompt &key require-match default)
  (let ((list (-filter (lambda (x) (not (s-starts-with? "." x))) (directory-files chatblade-config-directory))))
    (completing-read (or prompt "Select prompt file: ") list nil require-match nil nil default)))

(defun chatblade--select-query-template ()
  (if (= (length chatblade-query-template-alist) 0)
      (read-string "Input query template: ")
    (let* ((list (mapcar 'car chatblade-query-template-alist)))
      (completing-read "Select template or input here: " list nil nil nil nil))))

(defun chatblade--extract-conversations-from (session-file)
  (let* ((text (with-temp-buffer
                 (insert-file-contents session-file)
                 (buffer-string)))
         (conversations (yaml-parse-string text :object-type 'plist :sequence-type 'list :null-object nil :false-object nil)))
    (-filter (lambda (c) (not (string= (plist-get c :role) "system")))
             conversations)))

(defun chatblade--session-files (&key outdated)
  (let* ((last (chatblade-request "--session-path" :session "last" :omit-query nil))
         (dir (file-name-directory last))
         (entries (directory-files-and-attributes dir t chatblade-session-prefix))
         (now (time-convert (current-time) 'integer))
         (threshold-at (- now (* 60 60 24 chatblade-session-outdated-dates))))
    (-map
     (lambda (e) (nth 0 e))
     (-filter (lambda (e)
                (let ((updated-at (time-convert (nth 6 e) 'integer)))
                  (if outdated
                      (< updated-at threshold-at)
                    (> updated-at threshold-at))))
              entries))))

(defun chatblade--select-session-file (prompt-name)
  (let* ((files (chatblade--session-files :outdated nil))
         (files (-filter
                 (lambda (f)
                   (let ((p (chatblade--resolve-prompt-name-by-session (chatblade--session-from f))))
                     (or (not p) (string= p prompt-name))))
                 files))
         (alist (-map
                 (lambda (f)
                   (let* ((conversations (chatblade--extract-conversations-from f))
                          (c (-find (lambda (c) (string= (plist-get c :role) "user")) conversations))
                          (query (plist-get c :content))
                          (readable (truncate-string-to-width query 100 nil nil "…")))
                     (cons readable f)))
                 files))
         (readable (completing-read "Select session: " (-map 'car alist) nil t nil nil)))
    (assoc-default readable alist)))

(defun chatblade--session-from (session-file)
  (file-name-sans-extension (file-name-nondirectory session-file)))

(defun chatblade--new-session (prompt-name)
  (format "%s-%s-%s"
          chatblade-session-prefix
          (or prompt-name "nothing")
          (format-time-string "%Y%m%d%H%M%S")))

(defun chatblade--select-buffer ()
  (let* ((buffers (-filter (lambda (x) (eq (buffer-local-value 'major-mode x) 'chatblade-mode)) (buffer-list)))
         (cands (mapcar (lambda (x) (buffer-local-value 'chatblade--query x)) buffers))
         (answer (completing-read "Select buffer: " cands nil t nil nil)))
    (-find (lambda (x) (string= (buffer-local-value 'chatblade--query x) answer)) buffers)))

(defun chatblade--interactive-prompt-p ()
  (save-excursion
    (goto-char (pos-bol))
    (re-search-forward chatblade-interactive-prompt-regexp (pos-eol) t)))

(defun chatblade--goto-latest-interactive-prompt-beginning ()
  (goto-char (point-max))
  (while (not (chatblade--interactive-prompt-p))
    (forward-line -1))
  (goto-char (pos-bol)))

(defun chatblade--update-mode-name ()
  (let ((model (buffer-local-value 'chatblade--model (current-buffer))))
    (setq mode-name (format "Chat[%s]" (or model "-")))))

(defun chatblade--insert-reply (text)
  (insert text "\n\n"))

(defun chatblade--insert-query (text)
  (let ((pt (point)))
    (insert text "\n")
    (font-lock-append-text-property pt (point) 'font-lock-face 'font-lock-comment-face)
    (set-marker comint-last-input-start pt)
    (set-marker comint-last-input-end (point))))

(defun chatblade--resume-output-filter (proc string)
  (with-current-buffer (process-buffer proc)
    (let ((inhibit-read-only t))
      (goto-char (process-mark proc))
      (insert string)
      (set-marker (process-mark proc) (point))
      (when (chatblade--interactive-prompt-p)
        (goto-char (pos-bol))
        (delete-region (point-min) (point))
        (widen)
        (insert (format "Resumed with %s!\n" (or chatblade--model "model not specified")))
        (goto-char (point-max))
        (set-marker (process-mark proc) (point))
        (set-process-filter proc 'comint-output-filter)
        (set-process-sentinel proc #'chatblade--quit)))))

(defun chatblade--resume (proc event)
  (let ((buf (process-buffer proc)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (let* ((inhibit-read-only t)
               (session (buffer-local-value 'chatblade--session (current-buffer)))
               (model (buffer-local-value 'chatblade--model (current-buffer)))
               (args (chatblade--make-arguments nil nil session model "--interactive" "--only")))
          (chatblade--goto-latest-interactive-prompt-beginning)
          (narrow-to-region (point) (point-max))
          (delete-region (point) (point-max))
          (insert "Resuming...\n")
          (set-marker (process-mark proc) (point))
          (chatblade--update-mode-name)
          (apply #'make-comint-in-buffer "chatblade" (current-buffer) chatblade-executable-path nil args)
          (let ((proc (get-buffer-process (current-buffer))))
            (when proc (set-process-filter proc #'chatblade--resume-output-filter))))))))

(defun chatblade--restart (proc event)
  (let ((buf (process-buffer proc)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (let* ((inhibit-read-only t)
               (query (buffer-local-value 'chatblade--query (current-buffer)))
               (prompt-name (buffer-local-value 'chatblade--prompt-name (current-buffer)))
               (session (chatblade--new-session prompt-name))
               (model (buffer-local-value 'chatblade--model (current-buffer)))
               (args (chatblade--make-arguments query prompt-name session model "--interactive" "--only")))
          (erase-buffer)
          (chatblade--insert-query query)
          (chatblade--update-mode-name)
          (apply #'make-comint-in-buffer "chatblade" (current-buffer) chatblade-executable-path nil args)
          (let ((proc (get-buffer-process (current-buffer))))
            (when proc (set-process-sentinel proc #'chatblade--quit))))))))

(defun chatblade--quit (proc event)
  (let ((buf (process-buffer proc)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (insert (format "\nProcess %s %s\n" proc event)))
        (rename-buffer (concat " " (buffer-name)))))))

(defun chatblade--quit-buffer (buffer sentinel)
  (let ((proc (get-buffer-process buffer)))
    (when proc
      (set-process-sentinel proc sentinel)
      (comint-simple-send proc "quit"))))

(defun chatblade--new-buffer (prompt-name name)
  (let* ((name (replace-regexp-in-string (rx (+ (not (any "a-z" "A-Z" "0-9")))) "" name))
         (name (truncate-string-to-width name chatblade-buffer-name-truncate-width nil nil "…")))
    (with-current-buffer (generate-new-buffer (format "*chatblade:%s %s*" prompt-name name))
      (chatblade-mode)
      (current-buffer))))

(defun chatblade--start-buffer (query)
  (with-current-buffer (chatblade--new-buffer chatblade--prompt-name chatblade--template-name)
    (let* ((session (chatblade--new-session chatblade--prompt-name))
           (args (chatblade--make-arguments query chatblade--prompt-name session chatblade--model "--interactive" "--only")))
      (setq-local chatblade--query query)
      (setq-local chatblade--session session)
      (setq-local chatblade--model chatblade--model)
      (setq-local chatblade--prompt-name chatblade--prompt-name)
      (chatblade--insert-query query)
      (chatblade--update-mode-name)
      (apply #'make-comint-in-buffer "chatblade" (current-buffer) chatblade-executable-path nil args)
      (let ((proc (get-buffer-process (current-buffer))))
        (when proc (set-process-sentinel proc #'chatblade--quit)))
      (current-buffer))))

(defun chatblade--make-arguments (query prompt-name session model &rest args)
  (let* ((prompt-name (when prompt-name (list "--prompt-file" prompt-name)))
         (session (when session (list "--session" session)))
         (model (when model (list "--chat-gpt" model))))
    (when query (setq args `(,@args ,query)))
    `(,@prompt-name ,@session ,@chatblade-default-arguments ,@args)))

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
      (when (chatblade--interactive-prompt-p)
        (font-lock-append-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'comint-highlight-prompt)
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
  (switch-to-buffer-other-window (chatblade--start-buffer query)))

;;;###autoload
(cl-defun chatblade-request (query &key prompt-name session (omit-query t))
  (let* ((prompt-name (if prompt-name prompt-name (chatblade--resolve-prompt-name-by-mode major-mode)))
         (args (if omit-query '("--only") '()))
         (args (apply 'chatblade--make-arguments query prompt-name session chatblade-default-model args)))
    (message "[Chatblade] call : %s" args)
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
  "C-c C-r"    #'chatblade-switch-model
  "C-c C-S-r"  #'chatblade-switch-model-with-resume
  "C-c C-d"    #'chatblade-describe-current-session)

(define-derived-mode chatblade-mode comint-mode "Chat[?]"
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

(defun chatblade-switch-model-with-resume ()
  (interactive)
  (chatblade-switch-model t))

(defun chatblade-switch-model (resume)
  (interactive (list (if current-prefix-arg t nil)))
  (let* ((model chatblade-default-switched-model)
         (model (if (string= model chatblade--model) chatblade-default-model model))
         (model (if current-prefix-arg
                    (read-string (format "Model (%s): " model) nil nil model)
                  model))
         (sentinel (if resume #'chatblade--resume #'chatblade--restart)))
    (setq-local chatblade--model model)
    (chatblade--quit-buffer (current-buffer) sentinel)))

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
       (insert (chatblade-request "--session-dump" :session session :omit-query nil))
       (yaml-mode)
       (current-buffer)))))

;;;###autoload
(defun chatblade-start (prompt-name)
  (interactive (list (if current-prefix-arg nil (chatblade--resolve-prompt-name-by-mode major-mode))))
  (let* ((template-name (chatblade--select-query-template))
         (template (or (assoc-default template-name chatblade-query-template-alist)
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
         (chatblade--template-name template-name))
    (deactivate-mark)
    (funcall performer (format template query))))

;;;###autoload
(defun chatblade-switch-to-buffer (buffer)
  (interactive (list (chatblade--select-buffer)))
  (switch-to-buffer-other-window buffer))

;;;###autoload
(defun chatblade-resume (prompt-name model)
  (interactive (list (if current-prefix-arg nil (chatblade--resolve-prompt-name-by-mode major-mode))
                     (if current-prefix-arg chatblade-default-model chatblade-default-switched-model)))
  (let* ((session-file (chatblade--select-session-file prompt-name))
         (conversations (chatblade--extract-conversations-from session-file))
         (session (chatblade--session-from session-file))
         (query (plist-get
                 (-find (lambda (c) (string= (plist-get c :role) "user")) conversations)
                 :content))
         (prompt-name (chatblade--resolve-prompt-name-by-session session))
         (args (chatblade--make-arguments nil nil session model "--interactive" "--only")))
    (with-current-buffer (chatblade--new-buffer prompt-name query)
      (setq-local chatblade--query query)
      (setq-local chatblade--session session)
      (setq-local chatblade--model model)
      (setq-local chatblade--prompt-name prompt-name)
      (dolist (c conversations)
        (let ((content (plist-get c :content)))
          (case (plist-get c :role)
            ("user" (chatblade--insert-query content))
            ("assistant" (chatblade--insert-reply content)))))
      (let ((pt (point)))
        (insert "Resuming...\n")
        (narrow-to-region pt (point-max)))
      (chatblade--update-mode-name)
      (apply #'make-comint-in-buffer "chatblade" (current-buffer) chatblade-executable-path nil args)
      (let ((proc (get-buffer-process (current-buffer))))
        (when proc (set-process-sentinel proc #'chatblade--resume-output-filter))))))

;;;###autoload
(defun chatblade-clear-outdated-sessions ()
  (interactive)
  (dolist (file (chatblade--session-files :outdated t))
    (delete-file file)
    (message "[Chatblade] delete session : %s" file)))

;;;###autoload
(defun chatblade-update-prompt-file (thing name)
  (interactive
   (let* ((default-name (replace-regexp-in-string "-mode\\'" "" (symbol-name major-mode)))
          (default-thing (mapconcat 's-capitalize (split-string default-name "-") " ")))
     (list (read-string (format "What do you make a prompt for? (%s) " default-thing) nil nil default-thing)
           (chatblade--select-prompt-name
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
