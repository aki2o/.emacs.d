(bundle redo+)
(use-package redo+
  :commands (redo undo)
  :config
  (setq undo-no-redo t)
  (setq undo-limit 600000)
  (setq undo-strong-limit 900000))
