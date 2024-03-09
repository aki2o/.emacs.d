(bind-keys :map minibuffer-local-map
           ([remap keyboard-escape-quit] . minibuffer-keyboard-quit)
           ("C-M-p" . previous-matching-history-element)
           ("C-M-n" . next-matching-history-element))

(setq read-file-name-completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)
(setq completion-ignore-case t)


(use-package vertico
  :custom ((vertico-count 25))
  :init
  (add-hook 'minibuffer-setup-hook 'vertico-repeat-save)
  :config
  (vertico-mode)

  (define-key vertico-map (kbd "C-S-h") 'backward-kill-word)
  (define-key vertico-map (kbd "C-S-l") 'vertico-insert)
  (define-key vertico-map (kbd "M-j") 'vertico-next-group)
  (define-key vertico-map (kbd "M-k") 'vertico-previous-group)
  (define-key vertico-map (kbd "M-n") '~vertico-next-history-element)
  (define-key vertico-map (kbd "M-p") '~vertico-previous-history-element)

  (advice-add 'vertico--setup :after '~vertico-setup)
  (advice-add 'next-history-element :around '~vertico-scroll-up)
  (advice-add 'previous-history-element :around '~vertico-scroll-down))

(use-package vertico-repeat
  :config
  (add-to-list 'vertico-repeat-transformers '~vertico-repeat-transform-session-candidate t)
  (add-to-list 'vertico-repeat-filter 'read-file-name t)
  (add-to-list 'vertico-repeat-filter 'read-directory-name t)
  (add-to-list 'vertico-repeat-filter 'read-buffer t)
  
  (advice-add 'vertico-repeat--run :around '~vertico-let-current-session))

;; (define-key vertico-map (kbd "C-S-j") 'vertico-scroll-up)
;; (define-key vertico-map (kbd "C-S-k") 'vertico-scroll-down)
;; としたいのだけど、 next-history-element/previous-history-element が発動してしまう...
;; 原因がわかってなくて、ローカル変数とアドバイスで無理やり挙動を変えてる
(defvar ~vertico-scroll-activated nil)
(make-variable-buffer-local '~vertico-scroll-activated)

(defun ~vertico-next-history-element (n)
  (interactive "P")
  (let ((~vertico-scroll-activated nil))
    (call-interactively 'next-history-element)))

(defun ~vertico-previous-history-element (n)
  (interactive "P")
  (let ((~vertico-scroll-activated nil))
    (call-interactively 'previous-history-element)))

(defun ~vertico-scroll-up (orig &rest args)
  (if ~vertico-scroll-activated
      (call-interactively 'vertico-scroll-up)
    (apply orig args)))

(defun ~vertico-scroll-down (orig &rest args)
  (if ~vertico-scroll-activated
      (call-interactively 'vertico-scroll-down)
    (apply orig args)))

(defun ~vertico-setup ()
  (setq ~vertico-scroll-activated t))

;; vertico-repeat の実行中に、実行対象の session の情報にアクセスできるようにしてる
(defvar ~vertico-current-session nil)

(defun ~vertico-let-current-session (orig &rest args)
  (let ((~vertico-current-session (car args)))
    (apply orig args)))

;; vertico終了時に選択していた候補の情報が異なる毎に履歴は作らないようにする
(defun ~vertico-repeat-transform-session-candidate (session)
  (setf (nth 2 session) nil)
  session)


(use-package fussy
  :custom ((fussy-max-candidate-limit 30000)
           (fussy-score-fn 'fussy-fzf-native-score)
           (fussy-filter-fn 'fussy-filter-default))
  :init
  (setq completion-styles '(fussy))
  (add-hook 'after-init-hook 'fzf-native-load-dyn))


(use-package prescient
  :defer t
  :custom ((prescient-aggressive-file-save t))
  )


;; recent-file で、直近使ったものが上に来なくなってしまったので使うのやめてる
;;
;; (use-package vertico-prescient
;;   :after vertico
;;   :custom ((vertico-prescient-enable-filtering nil)
;;            (vertico-prescient-override-sorting t))
;;   :config
;;   (setq vertico-prescient-completion-styles completion-styles) ;; ensure to be done after configuring completion-styles
;;   (vertico-prescient-mode 1))


(use-package marginalia
  :bind (:map minibuffer-local-map
              ("C-;" . marginalia-cycle))
  :init
  (marginalia-mode))
