(defvar ~kmacro-save-file (concat user-emacs-directory "conf/99_saved_kmacro.el"))

(defun ~kmacro-save (symbol)
  "save last defined kbd macro."
  (interactive "SName for last kbd macro: ")
  (name-last-kbd-macro symbol)
  (with-current-buffer (find-file-noselect ~kmacro-save-file)
    (goto-char (point-max))
    (insert-kbd-macro symbol)
    (basic-save-buffer)))
