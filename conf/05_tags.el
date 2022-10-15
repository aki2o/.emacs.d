(defmacro ~tags-be-find-tag-command (command)
  `(defadvice ,command (around ~tags-insert-find-tag-ring activate)
     (require 'etags nil t)
     (let ((~m (point-marker)))
       ad-do-it
       (when (or (not (equal (current-buffer) (marker-buffer ~m)))
                 (not (eq (point) (marker-position ~m))))
         (ring-insert find-tag-marker-ring ~m)))))
