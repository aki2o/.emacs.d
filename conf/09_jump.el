(bundle point-undo)
(use-package point-undo
  :commands (point-undo point-redo))


(bundle goto-chg :depends (undo-tree))
(use-package goto-chg
  :commands (goto-last-change goto-last-change-reverse))


(~tags-be-find-tag-command find-function)
(~tags-be-find-tag-command find-variable)
