(define-global-minor-mode global-abbrev-mode
  abbrev-mode (lambda () (abbrev-mode 1)))

(setq abbrev-file-name (concat user-emacs-directory "abbrev_defs"))
(setq save-abbrevs t)
(quietly-read-abbrev-file)
(global-abbrev-mode 1)

(defun ~add-typo-resolve (arg)
  (interactive "p")
  (let* ((typo-text (if (>= arg 0)
                        (buffer-substring-no-properties
                         (point)
                         (if (= arg 0)
                             (mark)
                           (save-excursion (forward-word (- arg)) (point))))
                      (read-string "Typo value: ")))
         (resolve-text (read-string (format "Correct value for %s: " typo-text))))
    (set-text-properties 0 (length typo-text) nil typo-text)
    (define-abbrev global-abbrev-table typo-text resolve-text)))
