(setq xref-after-jump-hook '(recenter ~pulse-momentary))

(setq find-function-recenter-line nil)


(use-package point-undo
  :defer t
  :commands (point-undo point-redo))


(use-package undo-tree
  :defer t)


(use-package goto-chg
  :defer t
  :commands (goto-last-change goto-last-change-reverse))
