(bundle syl20bnr/emacs-emoji-cheat-sheet-plus :depends (helm))
(bundle emojify)
(use-package emoji-cheat-sheet-plus
  :defer t
  
  :config

  ;; p-r

  ;; create from emojify data
  
  (require 'emojify)
  (defun emoji-cheat-sheet-plus--create-cache ()
    "Create the image cache."
    (unless emoji-cheat-sheet-plus-image--cache
      (unless emojify-emojis
        (emojify-set-emoji-data))
      (setq emoji-cheat-sheet-plus-image--cache
            (loop for emoji-name being hash-keys in emojify-emojis using (hash-values emoji-hash)
                  for emoji-path = (concat emojify-image-dir "/" (gethash "image" emoji-hash))
                  for emoji-img = (create-image emoji-path 'png nil :ascent 'center)
                  collect (cons (intern emoji-name) emoji-img)))))

  )


(bundle company-emoji)
(use-package company-emoji
  :defer t
  :init
  
  (defvar ~company-emoji-setup-hooks
    '(org-mode-hook markdown-mode-hook git-commit-mode-hook))
  
  (dolist (hook ~company-emoji-setup-hooks)
    (add-hook hook 'company-emoji-init t))

  :config
  
  ;; p-r

  (require 'emojify)
  (defvar company-emoji--cached-list nil)
  (defun company-emoji--create-list ()
    (or company-emoji--cached-list
        (setq company-emoji--cached-list
              (cl-loop initially (unless emojify-emojis (emojify-set-emoji-data))
                       for emoji-name being hash-keys in emojify-emojis using (hash-values emoji-hash)
                       for codepoint = (gethash "unicode" emoji-hash)
                       if codepoint
                       collect (propertize emoji-name :unicode codepoint)))))
 )


;; (bundle ac-emoji)
;; (use-package ac-emoji
;;   :defer t
;;   :init

;;   (defvar ~ac-emoji-setup-hooks
;;     '(org-mode-hook markdown-mode-hook git-commit-mode-hook))
  
;;   (dolist (hook ~ac-emoji-setup-hooks)
;;     (add-hook hook 'ac-emoji-setup t))

;;   ;; p-r

;;   ;; create from emojify data
  
;;   (require 'emojify)
;;   (defvar ac-emoji--candidates
;;     (cl-loop initially (unless emojify-emojis
;;                          (emojify-set-emoji-data))
;;              for emoji-name being hash-keys in emojify-emojis using (hash-values emoji-hash)
;;              for codepoint = (gethash "unicode" emoji-hash)
;;              if codepoint
;;              collect (popup-make-item emoji-name :summary codepoint)))

;;   :config

;;   ;; emoji-fontset でやってるっぽい
;;   ;; (cond ((~is-mac)
;;   ;;        (set-fontset-font
;;   ;;         t 'symbol
;;   ;;         (font-spec :family "Apple Color Emoji") nil 'prepend))
;;   ;;       ((~is-windows)
;;   ;;        nil)
;;   ;;       (t
;;   ;;        (set-fontset-font
;;   ;;         t 'symbol
;;   ;;         (font-spec :family "Symbola") nil 'prepend)))
;;   )


(bundle emoji-fontset)
(use-package emoji-fontset
  :if window-system
  :commands (emoji-fontset-enable)
  :init
  (cond ((or (~is-mac) (~is-windows))
         (emoji-fontset-enable))
        (t
         (emoji-fontset-enable "Symbola"))))

