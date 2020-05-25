(require 'cl)
(require 'rx)
(require 'regexp-opt)
(require 'em-glob)
(require 'deferred)
(require 'auto-complete)

(defvar bashcmp-ident-re "[a-zA-Z_][a-zA-Z0-9_]*")
(defvar bashcmp-variable-re (rx-to-string `(and (group (regexp ,bashcmp-ident-re)) "=")))
(defvar bashcmp-local-re (rx-to-string `(and "local" (+ space))))
(defvar bashcmp-bashbuffer-re (rx-to-string `(and buffer-start (* space) "#!" (* (any not-newline)) "bash")))

(defvar bashcmp-lib-directorys nil)

(defun bashcmp-get-point-current-sentence-start ()
  (save-excursion
    (or (when (re-search-backward ";" (point-at-bol) t)
          (point))
        (point-at-bol))))

(defun bashcmp-current-sentence-localized-p ()
  (save-excursion
    (re-search-backward bashcmp-local-re (bashcmp-get-point-current-sentence-start) t)))

(defun bashcmp-get-variables-in-buffer (buff onlypub)
  (with-current-buffer buff
    (loop initially (goto-char (point-min))
          while (re-search-forward bashcmp-variable-re nil t)
          for v = (match-string-no-properties 1)
          when (or (not onlypub)
                   (not (bashcmp-current-sentence-localized-p)))
          collect v)))

(defun bashcmp-get-library-buffers ()
  )

(defun bashcmp-get-public-variables (mdl_or_name var-type &optional as-struct)
  (let* ((mdl (if (plcmp4ac-module-p mdl_or_name)
                     mdl_or_name
                   (plcmp4ac-get-module-info mdl_or_name))))
    (when (plcmp4ac-module-p mdl)
      (append (loop for v being the hash-values in (plcmp4ac-module-memberh mdl)
                    if (and (not (plcmp4ac-variable-lexical v))
                            (eq (plcmp4ac-variable-type v) var-type))
                    if as-struct collect v
                    else         append (list (plcmp4ac-variable-name v)
                                              (plcmp4ac-variable-fullname v)))
              (loop for m in (plcmp4ac-module-parents mdl)
                    append (plcmp4ac-get-public-variables-in-module m var-type as-struct))))))


(defun bashcmp-make-candidates ()
  )

(defun bashcmp-make-document-text (selected)
  )

(defvar ac-source-bashcmp-on-everywhere
  '((candidates . bashcmp-make-candidates)
    (document . bashcmp-make-document-text)
    (cache)))

(defvar ac-source-bashcmp-on-variable
  '((candidates . bashcmp-make-candidates)
    (prefix . "\$[a-zA-Z_0-9:]*")
    (symbol . "v")
    (document . bashcmp-make-document-text)
    (requires . 0)
    (cache)))


(provide 'bash-completion)