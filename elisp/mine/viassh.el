(eval-when-compile (require 'cl))
(require 'seq)
(require 'tramp-sh)

(defgroup viassh nil
  "Support to execute via ssh."
  :group 'convenience
  :prefix "viassh-")

(defcustom viassh-project-cache-file (concat user-emacs-directory ".viassh-project")
  "Filepath stores project configuration."
  :type 'string
  :group 'viassh)

(defcustom viassh-project-root-detect-function 'projectile-project-root
  "Function detect project root path for `default-directory'."
  :type 'symbol
  :group 'viassh)


(defvar viassh--activate nil)
(defvar viassh--project-cache-hash nil)

(defun viassh--project-cache (directory)
  (gethash (viassh--project-root-on directory) (viassh--ensure-project-cache-hash)))

(defun viassh--project-root-on (directory)
  (or (let ((default-directory directory))
        (funcall viassh-project-root-detect-function))
      (error "Can't detect project root path for %s" directory)))

(defun viassh--ensure-project-cache-hash ()
  (or viassh--project-cache-hash
      (setq viassh--project-cache-hash
            (or (viassh--load-project-cache-hash)
                (make-hash-table :test 'equal)))))

(defun viassh--load-project-cache-hash ()
  (when (file-exists-p viassh-project-cache-file)
    (read (with-temp-buffer
            (insert-file-contents viassh-project-cache-file)
            (buffer-string)))))

(defun viassh--store-project-cache (directory user-host)
  (puthash (viassh--project-root-on directory) user-host (viassh--ensure-project-cache-hash))
  (with-temp-buffer
    (insert (prin1-to-string (viassh--ensure-project-cache-hash)))
    (write-file viassh-project-cache-file)))

(defun* viassh--select-user-host-on (directory &key (use-cache t))
  (or (when use-cache (viassh--project-cache directory))
      (let* ((user-hosts (delq nil (cl-loop for e in (tramp-get-completion-function "ssh")
                                            append (funcall (nth 0 e) (nth 1 e)))))
             (candidates (seq-uniq (cl-loop for e in user-hosts
                                            for user = (nth 0 e)
                                            for host = (substring-no-properties (nth 1 e))
                                            collect (format "%s%s%s"
                                                            (or user "")
                                                            (if user "@" "")
                                                            host))))
             (selected (completing-read "select host: " candidates nil t nil '()))
             (user-host (split-string (or selected "") "@"))
             (user-host (if (= (length user-host) 1)
                            (list nil (nth 0 user-host))
                          user-host)))
        (viassh--store-project-cache directory user-host)
        user-host)))

(defun viassh-send-command (cmd)
  (let* ((user-host (viassh--select-user-host-on default-directory))
         (user (nth 0 user-host))
         (host (nth 1 user-host))
         (v (make-tramp-file-name :method "ssh" :user user :domain nil :host host :port nil :localname "/" :hop nil)))
    (with-current-buffer (tramp-get-connection-buffer v)
      (tramp-send-command v cmd)
      (buffer-string))))

(defun viassh-p ()
  viassh--activate)

(defmacro viassh (&rest body)
  `(let ((viassh--activate t))
     ,@body))

(defun viassh-configure-current-project ()
  (interactive)
  (viassh--store-project-cache default-directory nil))


(defadvice shell-command (around viassh activate)
  (if (not viassh--activate)
      ad-do-it
    (let ((command (ad-get-arg 0))
          (output-buf (or (ad-get-arg 1) (get-buffer-create "*Shell Command Output*"))))
      (with-current-buffer output-buf
        (erase-buffer)
        (insert (viassh-send-command command))
        (when (> (buffer-size) 0)
          (pop-to-buffer (current-buffer)))))))

(defadvice shell-command-to-string (around viassh activate)
  (if (not viassh--activate)
      ad-do-it
    (setq ad-return-value (viassh-send-command (ad-get-arg 0)))))


(provide 'viassh)
