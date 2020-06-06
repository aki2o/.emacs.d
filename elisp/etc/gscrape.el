
(eval-when-compile (require 'cl))
(require 'rx)
(require 'regexp-opt)
(require 'deferred)
(require 'concurrent)
(require 'url)
(require 'popup)
(require 'w3m)
(require 'log4e)

(defcustom gscrape:cache-directory "~/.emacs.d/gscrape")

(log4e:deflogger "gscrape"
                 "%t [%l] %m"
                 "%H:%M:%S"
                 '((fatal . "fatal")
                   (error . "error")
                   (warn  . "warn")
                   (info  . "info")
                   (debug . "debug")
                   (trace . "trace")))
(gscrape--log-set-level 'fatal 'trace)
(gscrape--log-set-coding-system 'utf-8-unix)


(defstruct gscrape--source name type config)

(defvar gscrape--source-dir-hash (make-hash-table :test 'equal))
(defvar gscrape--source-dir-index-file-name "index.txt")
(defvar gscrape--source-dir-index-buffer-name " *gscrape index*")


(defun gscrape:new-url-source (url))









(defvar gscrape--regexp-http-status (rx-to-string `(and bos "HTTP/" (+ (any "0-9.")) (+ space) (group (+ (any "0-9"))))))
(defvar gscrape--regexp-http-location (rx-to-string `(and bol "Location:" (+ space) (group (+ not-newline)) eol)))
(defun gscrape-do-scrape ()
  (interactive)
  (message "Downloading '%s' ..." "hoge")
  (deferred:$
    (deferred:timeout 6000 nil (deferred:url-retrieve "http://www.doraneko.org/xml/xpath10/19991008/"))
      (deferred:nextc it
        (lambda (buff)
          (if (not (buffer-live-p buff))
              (message "[GSCRAPE] failed download")
            (with-current-buffer buff
              (goto-char (point-min))
              (let* ((httpstat (or (when (re-search-forward gscrape--regexp-http-status nil t)
                                     (string-to-number (match-string-no-properties 1)))
                                   0)))
                    (cond ((and (>= httpstat 200)
                                (<  httpstat 300))
                           (goto-char (point-min))
                           (when (re-search-forward "\n\n" nil t)
                             (delete-region (point-min) (point)))
                           (goto-char (point-max))
                           (message "[GSCRAPE] start search")
                           (when (re-search-backward "contains" nil t)
                             (let* ((startpt (point))
                                    (html (when (re-search-forward "</p>" nil t)
                                            (buffer-substring startpt (point))))
                                    (text (when html
                                            (flet ((w3m-message (&rest args) nil))
                                              (with-temp-buffer
                                                (insert html)
                                                (w3m-region (point-min) (point-max))
                                                (buffer-string))))))
                               ;; (message "[GSCRAPE] html : %s" html)
                               ;; (message "[GSCRAPE] text : %s" text)
                                 (popup-tip "hogehoge"))))
                          ;; ((and (>= httpstat 300)
                          ;;       (<  httpstat 400)
                          ;;       (goto-char (point-min))
                          ;;       (re-search-forward "\n\n" nil t)
                          ;;       (re-search-backward genrnc--regexp-http-location nil t))
                          ;;  (let* ((redirecturl (match-string-no-properties 1)))
                          ;;    (genrnc--info "redirected [ %s ] to [ %s ]" accessurl redirecturl)
                          ;;    (genrnc--generate-from-url url fext rootp typeid force redirecturl)))
                          (t
                           (message "[GSCRAPE] http status failed")
                           )))))))))

