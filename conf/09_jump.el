(setq xref-after-jump-hook '(~pulse-momentary))

(use-package point-undo
  :defer t
  :commands (point-undo point-redo))


(use-package undo-tree
  :defer t)


(use-package goto-chg
  :defer t
  :commands (goto-last-change goto-last-change-reverse))


(when (fboundp '~tags-be-find-tag-command)
  (~tags-be-find-tag-command find-function)
  (~tags-be-find-tag-command find-variable))
