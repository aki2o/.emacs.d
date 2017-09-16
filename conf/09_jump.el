(bundle point-undo)
(use-package point-undo
  :bind (("M-," . point-undo)
         ("M-." . point-redo))
  )


(bundle goto-chg)
(use-package goto-chg
  :bind (("C-," . goto-last-change)
         ("C-." . goto-last-change-reverse))
  )


;; (~tags-be-find-tag-command find-function)
;; (~tags-be-find-tag-command find-variable)

