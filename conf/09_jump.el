(bundle point-undo)
(use-package point-undo
  :defer t
  :commands (point-undo point-redo))


(bundle undo-tree)
(use-package undo-tree
  :defer t)


(bundle goto-chg)
(use-package goto-chg
  :defer t
  :commands (goto-last-change goto-last-change-reverse))


(when (fboundp '~tags-be-find-tag-command)
  (~tags-be-find-tag-command find-function)
  (~tags-be-find-tag-command find-variable))

