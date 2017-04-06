;; (bundle ctxmenu)
;; (use-package ctxmenu-config
;;   :bind* (("C-*" . ctxmenu:show))

;;   :init
  
;;   (setq ctxmenu-config:exclude-features
;;         '(emacshelp move window region register kmacro buff anything helm))

;;   :config
  
;;   (ctxmenu-config:setup)

;;   (ctxmenu:add-source :menu-name "Forward"
;;                       :keystroke "M-f"
;;                       :menu-list 'ctxmenu:menu-list-flat)

;;   (ctxmenu:add-source :menu-name "Backward"
;;                       :keystroke "M-b"
;;                       :menu-list 'ctxmenu:menu-list-flat)

;;   ;; (ctxmenu:add-source :menu-name "Convert"
;;   ;;                     :keystroke "M-c"
;;   ;;                     :menu-list 'ctxmenu:menu-list-flat)

;;   (ctxmenu:add-source :menu-name "Search"
;;                       :keystroke "M-s"
;;                       :menu-list 'ctxmenu:menu-list-flat)

;;   (ctxmenu:add-source :menu-name "Exec"
;;                       :keystroke "C-x x"
;;                       :menu-list 'ctxmenu:menu-list-flat)

;;   (ctxmenu:add-source :menu-name "Anything"
;;                       :keystroke "C-x a"
;;                       :menu-list 'ctxmenu:menu-list-flat)

;;   )

