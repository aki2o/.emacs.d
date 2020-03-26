(bundle dockerfile-mode)
(use-package dockerfile-mode)


(bundle docker-tramp)
(use-package docker-tramp
  :init
  (setq docker-tramp-use-names t))


(use-package docker-run)
(use-package viassh)

(defun ~docker-context-p (buf)
  (let* ((filepath (expand-file-name (buffer-file-name buf))))
    (cond
     ((string-prefix-p (concat (expand-file-name "~/") "dev/mf") filepath)
      t)
     (t
      nil))))
