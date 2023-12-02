(use-package open-junk-file
  :defer t
  :commands (open-junk-file)
  :init
  (setq open-junk-file-format (concat user-emacs-directory "junk/%Y-%m-%d-%H%M%S.")))
