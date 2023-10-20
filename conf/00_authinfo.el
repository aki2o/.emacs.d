;; Emacs27 あたりから .authinfo.plist 保存でフリーズするようになってしまったので一旦コメントアウト

;; (require 'auth-source)

;; (add-to-list 'auth-sources (concat user-emacs-directory ".authinfo.gpg"))
;; (add-to-list 'auth-sources (concat user-emacs-directory ".authinfo.plist"))

;; (with-eval-after-load 'mmask
;;   (mmask-regist-path 'plstore-mode (concat user-emacs-directory ".authinfo.plist")))

;; (defadvice auth-source-search (before ~fix-system-name activate)
;;   (let ((host (ignore-errors (plist-get (ad-get-args 0) :host))))
;;     (when (and host (string= host (system-name)))
;;       (plist-put (ad-get-args 0) :host "localhost"))))


;; (defun ~auth-source-get-property (prop-name &rest spec &allow-other-keys)
;;   (let* ((founds (apply 'auth-source-search spec))
;;          (pkey (intern (concat ":" (format "%s" prop-name))))
;;          (ret (when founds (plist-get (nth 0 founds) pkey))))
;;     (if (functionp ret)
;;         (funcall ret)
;;       ret)))


;; (bundle oauth2)
;; (use-package oauth2
;;   :custom ((oauth2-token-file (concat user-emacs-directory ".oauth2.plstore"))))

