;; For avoiding error on start of gnus in Windows
(when (~is-windows)
  (defun gnutls-available-p () nil))

(setq ~browse-internal-url-list
      '("https://docs.ruby-lang.org/"))

(setq browse-url-browser-function
      `((,(concat "^" (regexp-opt ~browse-internal-url-list)) . ~browse-url)
        ("^dict://"                                           . browse-url-default-browser)
        ("."                                                  . ~browse-url-externally)))

(when (~is-windows)
  (setq browse-url-generic-program (w32-short-file-name "C:/Program Files/Mozilla Firefox/firefox.exe")))

(defun ~browse-url-externally (url &rest args)
  (interactive (browse-url-interactive-arg "URL: "))
  (cond ((~is-windows)
         ;; For Windows
         ;; browser-url-firefoxでURLを開くと、指定したURLのページがfirefox起動時に1度エラーが返って再実行し、
         ;; 結果2つ表示されるので、browse-url-genericを使うようにする。
         (browse-url-generic url))
        ((member system-type '(darwin))
         ;; For Mac OS X
         (cond ((= emacs-major-version 23)
                ;; Emacs 23では、browse-url-firefoxで起動させたり、単純にopenで起動すると、
                ;; 既にブラウザが実行しているとエラーになったりする。
                (ns-do-applescript (format (concat "tell application \"Safari\" to make document with properties {URL:\"%s\"}\n"
                                                   "tell application \"Safari\" to activate")
                                           url)))
               (t
                ;; それ以外
                (start-process (concat "open " url) nil "open" url))))
        (t
         ;; For Other
         (browse-url-firefox url))))

(setq shr-external-browser '~browse-url-externally)

(defun ~browse-url (url &optional external-p)
  (interactive (browse-url-interactive-arg "URL: "))
  (cond ((and external-p
              (y-or-n-p "Open External Browser?"))
         (~browse-url-externally url))
        ((or (> emacs-major-version 24)
             (and (= emacs-major-version 24) (>= emacs-minor-version 4)))
         (eww-browse-url url t))
        ((functionp w3m-browse-url)
         (w3m-browse-url url))
        (t
         (~browse-url-externally url))))

(defun ~browse-bookmark ()
  (interactive)
  (let ((helm-sources '((helm-firefox . helm-source-firefox-bookmarks)
                        (helm-hatena-bookmark . helm-hatena-bookmark:source)))
        (anything-sources '((anything-config . anything-c-source-w3m-bookmarks)
                            (anything-config . anything-c-source-firefox-bookmarks)
                            (anything-hatena-bookmark . anything-c-source-hatena-bookmark)))
        (gen-sources (lambda (al)
                       (loop for e in al
                             if (or (featurep (car e))
                                    (require (car e) nil t))
                             collect (cdr e)))))
    (loop for e in `((helm . ,helm-sources)
                     (anything . ,anything-sources))
          for srcs = (funcall gen-sources (cdr e))
          if srcs
          return (funcall (car e) :sources srcs))))


(use-package sha1-el
  :straight (:type built-in)
  :defer t)


(use-package anything-hatena-bookmark
  :defer t
  :init
  (setq anything-hatena-bookmark-requires-pattern 0)
  
  :config
  (defadvice anything-hatena-bookmark-get-dump (around ~use-sh activate)
    (if (not (executable-find "anything-hatena-bookmark-get-dump"))
        ad-do-it
      (let* ((id (read-string "Hatena ID: "))
             (cmd (format "anything-hatena-bookmark-get-dump %s" (shell-quote-argument id))))
        (shell-command cmd)))))


(use-package eww
  :defer t
  :init
  ;; (setq eww-search-prefix "https://duckduckgo.com/html/?k1=-1&kc=1&kf=-1&q=")
  (setq eww-search-prefix "https://www.google.co.jp/search?q=")
  
  :config
  (defvar ~eww-hl-words nil)

  (defun ~eww-search-initial-input ()
    (thing-at-point 'word))

  (defun ~eww-search-manualy ()
    (interactive)
    (let ((pophint-thing:enable-on-thing-at-point nil))
      (call-interactively '~eww-search)))
  
  (defun ~eww-search (query)
    (interactive (list
                  (read-string "Enter query: " (~eww-search-initial-input))))
    (setq ~eww-hl-words (split-string query "\\s-+"))
    (~browse-url (concat eww-search-prefix
                         (replace-regexp-in-string "\\s-+" "+" query))
                 (not current-prefix-arg)))
  
  (add-hook 'eww-after-render-hook
            '(lambda ()
               (loop for w in ~eww-hl-words do (highlight-regexp w))
               (setq ~eww-hl-words nil))
            t)
  
  (defvar ~eww-colorize nil)
  
  (defun ~shr-colorize-region (orig start end fg &optional bg &rest _)
    (when ~eww-colorize
      (funcall orig start end fg)))
  
  (advice-add 'shr-colorize-region :around '~shr-colorize-region)
  (advice-add 'eww-colorize-region :around '~shr-colorize-region)
  
  (defun ~eww-toggle-colorize ()
    (interactive)
    (setq-local ~eww-colorize (not ~eww-colorize))
    (eww-reload))
  
  (bind-keys :map eww-mode-map
              ("s" . ~eww-search-manualy)
              ("S" . ~eww-search)
              ("h" . ~scroll-right)
              ("j" . scroll-up)
              ("k" . scroll-down)
              ("l" . ~scroll-left)
              ("<" . eww-back-url)
              ("b" . eww-list-bookmarks)
              ("B" . eww-add-bookmark)
              ("E" . eww-browse-with-external-browser))

  (when (fboundp 'pophint-thing:advice-thing-at-point-function)
    (pophint-thing:advice-thing-at-point-function ~eww-search-initial-input)))
