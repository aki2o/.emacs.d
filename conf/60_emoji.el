(use-package emoji-cheat-sheet-plus
  :straight (:host github :repo "syl20bnr/emacs-emoji-cheat-sheet-plus")
  :defer t
  :init
  (use-package helm :defer t))


(use-package emojify
  :after (emoji-cheat-sheet-plus)
  :config
  ;; p-r

  ;; create from emojify data
  (defun emoji-cheat-sheet-plus--create-cache ()
    "Create the image cache."
    (unless emoji-cheat-sheet-plus-image--cache
      (unless emojify-emojis
        (emojify-set-emoji-data))
      (setq emoji-cheat-sheet-plus-image--cache
            (loop for emoji-name being hash-keys in emojify-emojis using (hash-values emoji-hash)
                  for emoji-path = (concat emojify-image-dir "/" (gethash "image" emoji-hash))
                  for emoji-img = (create-image emoji-path 'png nil :ascent 'center)
                  collect (cons (intern emoji-name) emoji-img))))))


(use-package company-emoji
  :after (emojify)
  :init
  (defvar ~company-emoji-setup-hooks
    '(org-mode-hook markdown-mode-hook git-commit-mode-hook))
  
  (dolist (hook ~company-emoji-setup-hooks)
    (add-hook hook 'company-emoji-init t))

  :config
  ;; p-r
  (defvar company-emoji--cached-list nil)
  (defun company-emoji--create-list ()
    (or company-emoji--cached-list
        (setq company-emoji--cached-list
              (cl-loop initially (unless emojify-emojis (emojify-set-emoji-data))
                       for emoji-name being hash-keys in emojify-emojis using (hash-values emoji-hash)
                       for codepoint = (gethash "unicode" emoji-hash)
                       if codepoint
                       collect (propertize emoji-name :unicode codepoint))))))


;; (use-package ac-emoji
;;   :after (emojify)
;;   :init
;;   (defvar ~ac-emoji-setup-hooks
;;     '(org-mode-hook markdown-mode-hook git-commit-mode-hook))
  
;;   (dolist (hook ~ac-emoji-setup-hooks)
;;     (add-hook hook 'ac-emoji-setup t))

;;   ;; p-r

;;   ;; create from emojify data
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


(use-package emoji-fontset
  :if window-system
  :defer t
  :commands (emoji-fontset-enable)
  :init
  (cond ((or (~is-mac) (~is-windows))
         (emoji-fontset-enable))
        (t
         (emoji-fontset-enable "Symbola"))))
