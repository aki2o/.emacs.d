(bundle embark)
(use-package embark
  :config
  (add-hook 'find-file-hook '~embark-setup t))

(defun ~embark-setup ()
  (add-to-list '~action-at-point-functions 'embark-act))


(bundle embark-consult :type github :pkgname "oantolin/embark")
(use-package embark-consult
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

