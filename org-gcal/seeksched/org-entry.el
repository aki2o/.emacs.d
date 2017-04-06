
(eval-when-compile (require 'cl))
(require 'org)

(defun print-org-entry ()
  (destructuring-bind (filepath) command-line-args-left
    (let ((buff (find-file-noselect filepath t))
          (coding-system-for-write 'utf-8-unix))
      (with-current-buffer buff
        (loop initially (goto-char (point-min))
              with pt = -1
              while (and (> (point) pt)
                         (= (point) (point-at-bol))
                         (re-search-forward "\\=\\* +" nil t))
              for entry = (buffer-substring-no-properties
                           (point)
                           (save-excursion
                             (re-search-forward "^ +:PROPERTIES:" nil t)
                             (goto-char (point-at-bol))
                             (point)))
              for entry = (replace-regexp-in-string "\\s-+" " " entry)
              for entry = (replace-regexp-in-string "\\`\\s-+" "" entry)
              for entry = (replace-regexp-in-string "\\s-+\\'" "" entry)
              do (princ (concat "### " entry "\n"))
              do (setq pt (point))
              do (org-forward-heading-same-level 1))))
    (kill-emacs)))

