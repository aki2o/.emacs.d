(use-package info
  :defer t
  :hook (Info-mode . ~info-mode-setup)
  :config
  (add-to-list 'Info-default-directory-list (expand-file-name (concat user-emacs-directory "info")))
  
  (defun ~info-mode-setup ()
    ;; Infoバッファはview-modeにする
    (view-mode 1)))


;; エラーになっちゃったので一旦コメントアウト
;; (use-package info+
;;   :defer t
;;   :after (info))
