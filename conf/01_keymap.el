(when (~is-mac)
  (setq mac-option-modifier 'hyper)
  (setq mac-command-modifier 'meta)
  (define-key global-map [?¥] [?\\]))


;; キーバインド用のマイナーモードを作成
(defvar ~keyjack-mode-map (make-sparse-keymap))

(define-minor-mode ~keyjack-mode "My Global Key"
  :init-value nil
  :lighter " KJ"
  :keymap ~keyjack-mode-map)

(defvar ~keyjack-define-with-global-set-key t)

(defadvice global-set-key (after ~set-keyjack-mode-map activate)
  (define-key global-map (ad-get-arg 0) (ad-get-arg 1))
  (when ~keyjack-define-with-global-set-key
    (define-key ~keyjack-mode-map (ad-get-arg 0) (ad-get-arg 1))))

(add-hook 'find-file-hook '~keyjack-mode)
(add-hook 'shell-mode-hook '~keyjack-mode)
(global-set-key (kbd "C-x C-j") '~keyjack-mode)


(use-package hydra
  :custom ((hydra-hint-display-type 'posframe))
  :init
  (use-package posframe :defer t)
  :config
  (defvar ~hydra-help-delay 1.5)
  (plist-put hydra-posframe-show-params :poshandler 'posframe-poshandler-frame-bottom-right-corner)
  (plist-put hydra-posframe-show-params :background-color "gray30")
  (plist-put hydra-posframe-show-params :internal-border-width 10))
