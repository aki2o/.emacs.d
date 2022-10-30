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

(defvar ~browse-document-url-functions '())
(make-variable-buffer-local '~browse-document-url-functions)

(defun ~browse-document ()
  (interactive)
  (if (not ~browse-document-url-functions)
      (error "No ~browse-document-url-functions")
    (let ((~browse-search-url-function '(lambda (words)
                                          (let ((urls (mapcar (lambda (x) (funcall x words)) ~browse-document-url-functions)))
                                            (cond ((= (length urls) 1) (nth 0 urls))
                                                  (t (completing-read "URL: " urls)))))))
      (call-interactively '~browse-search))))

(cl-defmacro ~browse-document-defun (name baseurl &key (body nil) (internal nil) (path ""))
  (declare (indent 2))
  `(progn
     (when ,internal
       (add-to-list '~browse-internal-url-list ,baseurl t))
     (defun ,(intern (format "~browse-%s-document" name)) (words)
       (concat ,baseurl ,path (if (and (= (length words) 1) (string= (nth 0 words) "")) "" ,body)))))


(use-package eww
  :commands (~browse-search)
  :bind (:map eww-mode-map
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
              ("t" . ~eww-toggle-colorfully))
  :init
  ;; (setq eww-search-prefix "https://duckduckgo.com/html/?k1=-1&kc=1&kf=-1&q=")
  (setq eww-search-prefix "https://www.google.co.jp/search?q=")

  :config
  (~add-setup-hook 'eww-mode
    (face-remap-add-relative 'default :background "white" :foreground "black" :height 140)
    (face-remap-add-relative 'shr-link :foreground "linkColor")
    (setq cursor-type 'box)

    (when ~browse-searched-words
      (rename-buffer (format "*eww: %s*" (mapconcat 'identity ~browse-searched-words " ")) t))

    (setq-local shr-put-image-function 'shr-put-image-alt))

  (~add-setup-hook 'eww-after-render
    (loop for w in ~browse-searched-words do (highlight-regexp w))
    (setq ~browse-searched-words nil))

  (advice-add 'display-graphic-p :around '~shr-graphic)
  (advice-add 'shr-colorize-region :around '~shr-colorize-region)
  (advice-add 'eww-colorize-region :around '~shr-colorize-region))

(defvar ~eww-colorful-view nil)
(make-variable-buffer-local '~eww-colorful-view)

(defun ~shr-graphic (orig &rest args)
  (when (or (not (eq major-mode 'eww-mode))
            ~eww-colorful-view)
    (apply orig args)))

(defun ~shr-colorize-region (orig start end fg &optional bg &rest _)
  (when ~eww-colorful-view
    (funcall orig start end fg)))

(defun ~eww-toggle-colorfully ()
  (interactive)
  (setq ~eww-colorful-view (not ~eww-colorful-view))
  (eww-reload))

