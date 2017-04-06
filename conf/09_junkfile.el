(bundle open-junk-file)
(use-package open-junk-file
  :commands (open-junk-file)
  :init
  (setq open-junk-file-format (concat user-emacs-directory "junk/%Y-%m-%d-%H%M%S.")))

