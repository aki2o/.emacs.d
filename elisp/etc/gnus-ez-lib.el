(require 'gnus)

(defun gnus-ez-lib:get-default-server ()
  (nth 1 gnus-select-method))

(defun gnus-ez-lib:get-default-prefix ()
  (concat (symbol-name (nth 0 gnus-select-method))
          "+"
          (gnus-ez-lib:get-default-server)
          ":"))

(defun gnus-ez-lib:get-group-prefix (group)
  (let ((prefix (gnus-group-real-prefix group)))
    (if (and prefix
             (not (string= prefix "")))
        prefix
      (gnus-ez-lib:get-default-prefix))))

(defun gnus-ez-lib:has-prefix-p (group)
  (let ((prefix (gnus-group-real-prefix group)))
    (and prefix
         (not (string= prefix "")))))

(defun gnus-ez-lib:get-group-full-name (group)
  (if (gnus-ez-lib:has-prefix-p group)
      group
    (concat (gnus-ez-lib:get-default-prefix) group)))

(defun gnus-ez-lib:get-group-method/server (group)
  (let* ((prefix (gnus-ez-lib:get-group-prefix group))
         (plus (when prefix (string-match "+" prefix)))
         (mtd (or (when plus (substring prefix 0 plus))
                  (when prefix (replace-regexp-in-string ":\\'" "" prefix))))
         (mtd (when (and mtd (string-match "\\`[a-z]+\\'" mtd))
                mtd))
         (svr (when plus (substring prefix (+ plus 1) (- (length prefix) 1))))
         (svr (when (and svr (not (string= svr "")))
                svr)))
    (list mtd svr)))


(provide 'gnus-ez-lib)
