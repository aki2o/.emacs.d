;; ;; ttpで始まるURLをhttpとして認識させる
;; (setq thing-at-point-url-regexp
;;       (concat
;;        "\\<\\(h?ttps?://\\|ftp://\\|gopher://\\|telnet://"
;;        "\\|wais://\\|file:/\\|s?news:\\|mailto:\\)"
;;        thing-at-point-url-path-regexp))

;; (setq ffap-url-regexp
;;       (concat
;;        "\\`\\("
;;        "news\\(post\\)?:\\|mailto:\\|file:"
;;        "\\|"
;;        "\\(ftp\\|h?ttps?\\|telnet\\|gopher\\|www\\|wais\\)://"
;;        "\\)."))

;; (setq mime-browse-url-regexp
;;       (concat "\\(h?ttps?\\|ftp\\|file\\|gopher\\|news\\|telnet\\|wais\\|mailto\\):"
;;               "\\(//[-a-zA-Z0-9_.]+:[0-9]*\\)?"
;;               "[-a-zA-Z0-9_=?#$@~`%&*+|\\/.,]*[-a-zA-Z0-9_=#$@~`%&*+|\\/]"))

;; (defadvice thing-at-point-url-at-point (after support-omitted-h activate)
;;   (when (and ad-return-value (string-match "\\`ttps?://" ad-return-value))
;;     (setq ad-return-value (concat "h" ad-return-value))))

;; (defadvice ffap-url-at-point (after support-omitted-h activate)
;;   (when (and ad-return-value (string-match "\\`ttps?://" ad-return-value))
;;     (setq ad-return-value (concat "h" ad-return-value))))

;; (defadvice browse-url (before support-omitted-h (url &rest args) activate)
;;   (when (and url (string-match "\\`ttps?://" url))
;;     (setq url (concat "h" url))))

