(use-package dockerfile-mode
  :defer t
  :mode (("/Dockerfiles/" . dockerfile-mode)))


(use-package docker-tramp
  :defer t
  :init
  (setq docker-tramp-use-names t))


(use-package docker-run
  :straight (:type built-in)
  :defer t
  :commands (docker-run:exec
             docker-run:exec-viassh
             docker-run:configure-current-project
             docker-run:configure-current-project-viassh))


(use-package viassh
  :straight (:type built-in))


(defun* ~dockerize-shell-command (cmd &key (buffer (current-buffer)))
  (if (~docker-context-p buffer)
      (docker-run:exec-viassh 'shell-command (format "/bin/bash -l -c '%s'" cmd))
    (shell-command cmd)))

(defun ~docker-context-p (buf)
  (let* ((filepath (expand-file-name (buffer-file-name buf))))
    (cond
     ((string-prefix-p (concat (expand-file-name "~/") "dev/mf") filepath)
      t)
     (t
      nil))))
