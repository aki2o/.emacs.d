(require 'org)

(org-add-link-type "github-wiki")

(defun org-github-wiki-complete-link (&optional arg)
  (replace-regexp-in-string "\\`file:" "github-wiki:" (org-file-complete-link arg)))

(defadvice org-md-link (around org-link-github-wiki/mk-link activate)
  (if (not (string= (org-element-property :type (ad-get-arg 0)) "github-wiki"))
      ad-do-it
    (let* ((value (org-element-property :path (ad-get-arg 0)))
           (value (replace-regexp-in-string "\\.[^.]+\\'" "" value)))
      (setq ad-return-value (format "[[%s|%s]]" (ad-get-arg 1) value)))))

(provide 'org-link-github-wiki)
