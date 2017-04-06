(defun ~case-invoke (func)
  (let* ((startpt (if (region-active-p)
                      (region-beginning)
                    (save-excursion (forward-word 1) (backward-word 1) (point))))
         (endpt (if (region-active-p)
                    (region-end)
                  (save-excursion (forward-word 1) (point))))
         (rep (funcall func (buffer-substring startpt endpt))))
    (save-excursion
      (kill-region startpt endpt)
      (goto-char startpt)
      (insert rep))))


(defun ~case-upper ()
  (interactive)
  (call-interactively
   (if (region-active-p) 'upcase-region 'upcase-word)))

(defun ~case-lower ()
  (interactive)
  (call-interactively
   (if (region-active-p) 'downcase-region 'downcase-word)))

(defun ~case-capitalize ()
  (interactive)
  (call-interactively
   (if (region-active-p) 'capitalize-region 'capitalize-word)))

(defun ~case-capitalize-from-snake ()
  (interactive)
  (~case-invoke 's-upper-camel-case))

(defun ~case-snake ()
  (interactive)
  (~case-invoke 's-snake-case))


(unbind-key "M-c")
(bind-keys* ("M-c u" . ~case-upper)
            ("M-c l" . ~case-lower)
            ("M-c c" . ~case-capitalize-from-snake)
            ("M-c C" . ~case-capitalize)
            ("M-c s" . ~case-snake))

