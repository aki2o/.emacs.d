(require 'auth-source)

(add-to-list 'auth-sources (concat user-emacs-directory ".authinfo.gpg"))
(add-to-list 'auth-sources (concat user-emacs-directory ".authinfo.plist"))

(defadvice auth-source-search (before ~fix-system-name activate)
  (let ((host (ignore-errors (plist-get (ad-get-args 0) :host))))
    (when (and host (string= host (system-name)))
      (plist-put (ad-get-args 0) :host "localhost"))))


(defun ~auth-source-get-property (prop-name &rest spec &allow-other-keys)
  (let* ((founds (apply 'auth-source-search spec))
         (pkey (intern (concat ":" (format "%s" prop-name))))
         (ret (when founds (plist-get (nth 0 founds) pkey))))
    (if (functionp ret)
        (funcall ret)
      ret)))


(bundle oauth2)
(use-package oauth2
  :init
  (setq oauth2-token-file (concat user-emacs-directory ".oauth2.plstore")))

