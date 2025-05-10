(use-package dap-mode
  :defer t
  :config
  ;; (setq dap-print-io t)
  )

;; (require 'dap-node)

;; (defun my:dap-typescript ()
;;   (interactive)
;;   (let ((root (projectile-project-root)))
;;     (dap-debug
;;      (list :type "node"
;;            :request "launch"
;;            :name "typescript"
;;            :outDir "dist"
;;            :trace t
;;            ;; :program (concat root "node_modules/.bin/jest")
;;            :program (expand-file-name (buffer-file-name (current-buffer)))
;;            ;; :args (list (~projectile-relative-path (current-buffer)))
;;            :cwd root
;;            :runtimeExecutable "pnpm"
;;            :runtimeArgs (list "test")
;;            :protocol "inspector"
;;            :console "integratedTerminal"))))
