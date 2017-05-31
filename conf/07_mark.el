(setq mark-ring-max 16)
(setq global-mark-ring-max 256)

(defun ~set-mark-only ()
  (interactive)
  (push-mark nil t nil))

