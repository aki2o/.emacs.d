(defvar ~browse-internal-url-list '())

(setq browse-url-browser-function '~browse-url-browser)

(setq shr-external-browser '~browse-url-externally)

(defun ~browse-url-browser (url &rest args)
  (apply
   (cond ((string-match (concat "^" (regexp-opt ~browse-internal-url-list)) url)
          '~browse-url-internally)
         (t
          '~browse-url-externally))
   url args))

(defun ~browse-url-internally (url &rest args)
  (interactive (browse-url-interactive-arg "URL: "))
  (eww-browse-url url t))

(defun ~browse-url-externally (url &rest args)
  (interactive (browse-url-interactive-arg "URL: "))
  (start-process (concat "open " url) nil "open" url))

(defvar ~browse-searched-words nil)
(defvar ~browse-search-browser-function 'browse-url)
(defvar ~browse-search-url-function '~browse-search-url-with)
(defvar ~browse-document-url-functions-alist '())

(defun ~browse-search-initial-input ()
  (thing-at-point 'word))

(defun ~browse-search-url-with (words)
  (concat eww-search-prefix (mapconcat 'identity words "+")))

(defun ~browse-search (query)
  (interactive (list (read-string "Enter query: " (~browse-search-initial-input))))
  (setq ~browse-searched-words (split-string query "\\s-+"))
  (funcall ~browse-search-browser-function (funcall ~browse-search-url-function ~browse-searched-words)))

(defun ~browse-search-internally ()
  (interactive)
  (let ((~browse-search-browser-function '~browse-url-internally))
    (call-interactively '~browse-search)))

(defun ~browse-document ()
  (interactive)
  (let ((~browse-search-url-function '(lambda (words)
                                        (let* ((urls (cl-loop with mode = major-mode
                                                              while mode
                                                              for functions = (assoc-default
                                                                               (intern-soft (replace-regexp-in-string "-mode$" "" (symbol-name mode)))
                                                                               ~browse-document-url-functions-alist)
                                                              if functions
                                                              append (mapcar (lambda (f) (funcall f words)) functions)
                                                              do (setq mode (get mode 'derived-mode-parent)))))
                                          (cond ((not urls)
                                                 (error "No entries in ~browse-document-url-functions-alist for %s" key))
                                                ((= (length urls) 1) (nth 0 urls))
                                                (t (completing-read "URL: " urls)))))))
    (call-interactively '~browse-search)))

(cl-defmacro ~browse-document-defun-for (mode baseurl &key (name nil) (body nil) (internal t) (path ""))
  (declare (indent 2))
  (let ((f (intern (format "~browse-%s%s-document" mode (if name (format "-%s" name) "")))))
    `(progn
       (defun ,f (words)
         (concat ,baseurl ,path (if (and (= (length words) 1) (string= (nth 0 words) "")) "" ,body)))
       (when ,internal
         (add-to-list '~browse-internal-url-list ,baseurl t))
       (let ((entry (assoc ',mode ~browse-document-url-functions-alist)))
         (if entry
             (setf (cdr entry) (append (cdr entry) (list ',f)))
           (push (cons ',mode (list ',f)) ~browse-document-url-functions-alist))))))

(defvar ~browse-github-repositories '())

(defun ~browse-github-code (repo path words)
  (interactive
   (list (completing-read "Repo: " ~browse-github-repositories nil nil)
         (read-string "Path: " (ignore-errors (concat "*." (file-name-extension (buffer-file-name)))))
         (list (read-string "Enter query: " (~browse-search-initial-input)))))
  (let* ((url (format "https://github.com/search?q=%s%s+%s&type=code"
                      (concat "repo%3A" (replace-regexp-in-string "/" "%2F" repo))
                      (when path (concat "+path%3A" path))
                      (mapconcat 'identity words "+"))))
    (browse-url url)))


(use-package eww
  :commands (~browse-search)
  :custom (
           ;; (eww-retrieve-command '("chromium" "--headless" "--dump-dom"))
           )
  :init
  (setq eww-search-prefix "https://www.google.co.jp/search?q=")

  :config
  (bind-keys :map eww-mode-map
             ("j" . next-line)
             ("k" . previous-line)
             ("h" . ~scroll-right)
             ("l" . ~scroll-left)
             ("J" . ~scroll-up)
             ("K" . ~scroll-down)
             ("H" . eww-back-url)
             ("s" . ~browse-search-internally)
             ("S" . pophint-thing:just-~browse-search-internally)
             ("b" . eww-switch-to-buffer)
             ("o" . eww-browse-with-external-browser)
             ("R" . eww-reload)
             ("A" . ~eww-toggle-readable)
             ("T" . ~eww-toggle-font))

  (~add-setup-hook 'eww-mode
    (setq-local cursor-type 'box)
    (setq-local shr-put-image-function '~shr-put-image-alt)
    (setq-local shr-use-fonts nil)

    (when ~browse-searched-words
      (rename-buffer (format "*eww: %s*" (mapconcat 'identity ~browse-searched-words " ")) t)))

  (~add-setup-hook 'eww-after-render
    (setq ~browse-searched-words nil)
    (when ~eww-readable (eww-readable))
    (loop for w in ~browse-searched-words do (highlight-regexp w)))

  (with-eval-after-load 'owdriver
    (add-to-list 'owdriver-keep-driving-command-prefixes "eww-" t)
    (add-to-list 'owdriver-keep-driving-commands '~eww-toggle-readable t)
    (add-to-list 'owdriver-keep-driving-commands '~eww-toggle-font t)))

(defvar ~eww-readable t)

(defun ~shr-put-image-alt (spec alt &optional flags)
  (insert alt))

(defun ~eww-toggle-readable ()
  (interactive)
  (setq-local ~eww-readable (not ~eww-readable))
  (eww-reload))

(defun ~eww-toggle-font ()
  (interactive)
  (setq-local shr-use-fonts (not shr-use-fonts))
  (eww-reload))
