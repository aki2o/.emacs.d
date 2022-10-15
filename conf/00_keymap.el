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
(global-set-key (kbd "C-x j") '~keyjack-mode)


(bundle! bind-key)
(bundle posframe)


(bundle hydra)
(use-package hydra
  :defer t
  :custom ((hydra-hint-display-type 'posframe))
  :config
  (require 'posframe)
  (defvar ~hydra-help-delay 1.5)
  (plist-put hydra-posframe-show-params :poshandler 'posframe-poshandler-window-bottom-left-corner)
  (plist-put hydra-posframe-show-params :background-color "gray30")
  (plist-put hydra-posframe-show-params :internal-border-color "gray30")
  (plist-put hydra-posframe-show-params :internal-border-width 10))


(bundle which-key)
(use-package which-key
  :custom ((which-key-idle-delay 1.5))
  :init
  (which-key-mode)
  :config
  (which-key-add-key-based-replacements
   "<f1>" "Help"
   "M-m" "E2WM"
   "C-x a" "Abbrev"
   "C-x n" "Narrow"
   "C-x r" "Rectangle"
   "C-x v" "Version Control"
   "C-x p" "Perspb"
   "C-x C-k" "Kmacro"
   "C-x C-r" "CUA"
   "C-x C-t" "Tail"
   "C-x RET" "Coding system"
   "C-x ESC" "Repeat complex"
   "C-x 4" "Other window"
   "C-x 5" "Other frame"
   "C-x 6" "2C"
   "C-c" "Special Provided"))


(bundle which-key-posframe)
(use-package which-key-posframe
  :custom ((which-key-posframe-poshandler 'posframe-poshandler-window-bottom-left-corner)
           (which-key-posframe-border-width 10))
  :after (which-key)
  :config
  (require 'posframe)
  (which-key-posframe-mode)

  (custom-set-faces
   '(which-key-posframe ((t :inherit default :background "gray30")))
   '(which-key-posframe-border ((t (:inherit default :background "gray30")))))

  (defun which-key-posframe--max-dimensions (_)
    (cons (1- (frame-height)) (/ (frame-width) 2)))
  )

