;; cpanm RPC::EPC::Service DBI DBD::SQLite DBD::Pg DBD::mysql
(use-package edbi
  :defer t
  :commands (edbi:open-db-viewer))


(use-package e2wm-edbi
  :straight (:host github :repo "kiwanami/emacs-edbi")
  :defer t
  :config
  (setq e2wm:c-edbi-recipe
        '(| (:left-size-ratio 0.3)
            database
            (- (:upper-size-ratio 0.2)
               editor result)))

  (setq e2wm:c-edbi-winfo
        '((:name database)
          (:name editor)
          (:name result)))
  
  (e2wm:pst-class-register 
   (make-e2wm:$pst-class
    :name   'edbi
    :title  "DB"
    :init   'e2wm:dp-edbi-init
    :start  'e2wm:dp-edbi-start
    :leave  'e2wm:dp-edbi-leave
    :main   'database
    :switch 'e2wm:dp-edbi-switch
    :popup  'e2wm:dp-edbi-popup
    :keymap 'e2wm:dp-edbi-minor-mode-map))
  
  (e2wm:add-keymap
   e2wm:dp-edbi-minor-mode-map 
   '(("prefix n" . edbi:open-db-viewer)
     ) e2wm:prefix-key))
