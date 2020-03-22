;; For avoiding error on start of gnus in Windows
(when (~is-windows)
  (defun gnutls-available-p () nil))

(setq ~browse-external-url-list
      '("http://www.google.com/reader"
        "http://www.google.co.jp/reader"
        "http://maps.google.co.jp"
        "http://map.yahoo.co.jp"
        "http://map.labs.goo.ne.jp"
        "http://www.haloscan.com"
        "http://sitemeter.com"
        "http://www.hmv.co.jp"
        "https://www.facebook.com/"
        "https://mf.esa.io/"))

(setq browse-url-browser-function
      `((,(concat "^" (regexp-opt ~browse-external-url-list)) . ~browse-url-externally)
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
         (eww-browse-url url))
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


(bundle sha1-el :url "http://stuff.mit.edu/afs/sipb/contrib/emacs/packages/flim-1.14.7/sha1-el.el")
(use-package sha1-el
  :defer t)


(bundle k1LoW/anything-hatena-bookmark)
(use-package anything-hatena-bookmark
  
  :init
  
  (setq anything-hatena-bookmark-requires-pattern 0)
    
  :config
  
  (defadvice anything-hatena-bookmark-get-dump (around ~use-sh activate)
    (if (not (executable-find "anything-hatena-bookmark-get-dump"))
        ad-do-it
      (let* ((id (read-string "Hatena ID: "))
             (cmd (format "anything-hatena-bookmark-get-dump %s" (shell-quote-argument id))))
        (shell-command cmd))))
  
  )


(use-package eww
  
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

  (use-package pophint
    :config
    (pophint-thing:advice-thing-at-point-function ~eww-search-initial-input))
  
  )


;; (use-package w3m-load

;;   :init
  
;;   (setq w3m-init-file (concat user-emacs-directory "w3m/init.el"))
;;   (setq w3m-bookmark-file (concat user-emacs-directory "w3m/bookmark.html"))
;;   (setq w3m-home-page "http://www.google.co.jp/")
;;   (setq w3m-search-default-engine "google")
;;   (setq w3m-use-tab t)                       ; タブを使うかどうか
;;   (setq browse-url-new-window-flag t)        ; 新しいタブを開くかどうか
;;   (setq w3m-use-form t)                      ; フォームに入力可能とするかどうか
;;   (setq w3m-use-cookies t)                   ; Cookieを有効にするかどうか
;;   (setq w3m-default-display-inline-images t) ; 画像を表示するかどうか
;;   (setq w3m-favicon-cache-expire-wait nil)   ; Faviconのキャッシュを削除するかどうか

;;   :config
  
;;   (defun ~w3m-scroll-up ()
;;     (interactive)
;;     (w3m-scroll-up 5))

;;   (defun ~w3m-scroll-down ()
;;     (interactive)
;;     (w3m-scroll-down 5))

;;   (defun ~w3m-goto-next-url ()
;;     (interactive)
;;     (when w3m-next-url
;;       (w3m-keep-region-active)
;;       (let ((w3m-prefer-cache t))
;;         (w3m-history-store-position)
;;         (w3m-goto-url w3m-next-url))))

;;   (defun ~w3m-goto-previous-url ()
;;     (interactive)
;;     (when w3m-previous-url
;;       (w3m-keep-region-active)
;;       (let ((w3m-prefer-cache t))
;;         (w3m-history-store-position)
;;         (w3m-goto-url w3m-previous-url))))

;;   (defun ~w3m-view-current-url-with-externally ()
;;     (interactive)
;;     (w3m-view-url-with-external-browser w3m-current-url))

;;   (defun ~w3m-set-keys ()
;;     (local-set-key (kbd "j") '~w3m-scroll-up)
;;     (local-set-key (kbd "k") '~w3m-scroll-down)
;;     (local-set-key (kbd "h") 'w3m-scroll-left)
;;     (local-set-key (kbd "l") 'w3m-scroll-right)
;;     (local-set-key (kbd "J") 'w3m-scroll-up)
;;     (local-set-key (kbd "K") 'w3m-scroll-down)
;;     (local-set-key (kbd "H") 'w3m-view-previous-page)
;;     (local-set-key (kbd "L") 'w3m-view-next-page)
;;     (local-set-key (kbd "b") 'w3m-switch-buffer)
;;     (local-set-key (kbd "B") 'anything-w3m-bookmarks)
;;     (local-set-key (kbd "M") '~w3m-view-current-url-with-externally)
;;     (local-set-key (kbd "s") 'w3m-submit-form)
;;     (local-set-key (kbd "q") 'w3m-delete-buffer)
;;     (local-unset-key (kbd "g"))
;;     (local-set-key (kbd "g h") 'w3m-gohome)
;;     (local-set-key (kbd "g u") 'w3m-view-parent-page)
;;     (local-set-key (kbd "g a") 'beginning-of-buffer)
;;     (local-set-key (kbd "g z") 'end-of-buffer)
;;     (local-set-key (kbd "g n") '~w3m-goto-next-url)
;;     (local-set-key (kbd "g p") '~w3m-goto-previous-url)
;;     (local-set-key (kbd "g s") 'w3m-search)
;;     (local-unset-key (kbd ":"))
;;     (local-set-key (kbd ": h") 'w3m-history)
;;     (local-set-key (kbd ": c") 'w3m-cookie)
;;     (local-set-key (kbd ": t") 'w3m-create-empty-session)
;;     (local-set-key (kbd ": b") 'w3m-bookmark-view-new-session)
;;     (local-set-key (kbd "M-n") 'w3m-next-form)
;;     (local-set-key (kbd "M-p") 'w3m-previous-form)
;;     (local-set-key (kbd "M-]") 'w3m-next-buffer)
;;     (local-set-key (kbd "M-[") 'w3m-previous-buffer)
;;     (local-unset-key (kbd "M-k"))
;;     (local-unset-key (kbd "C-t"))
;;     )

;;   (add-hook 'w3m-mode-hook '~w3m-set-keys t)

;;   ;; Migemoを有効にする
;;   (when (and (featurep 'migemo)
;;              (featurep 'anything-migemo))
;;     (anything-migemize-command w3m-switch-buffer))

;;   )

