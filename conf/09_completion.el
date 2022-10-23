(define-key minibuffer-local-map (kbd "C-M-p") 'previous-matching-history-element)
(define-key minibuffer-local-map (kbd "C-M-n") 'next-matching-history-element)


(bundle vertico)
(use-package vertico
  :custom ((vertico-count 25))
  :init
  (add-hook 'minibuffer-setup-hook 'vertico-repeat-save)
  :config
  (vertico-mode)
  (define-key vertico-map (kbd "C-S-h") 'vertico-directory-delete-word)
  (define-key vertico-map (kbd "C-S-l") 'vertico-insert)
  (define-key vertico-map (kbd "M-j") 'vertico-next-group)
  (define-key vertico-map (kbd "M-k") 'vertico-previous-group)
  (define-key vertico-map (kbd "M-n") '~vertico-next-history-element)
  (define-key vertico-map (kbd "M-p") '~vertico-previous-history-element)

  (advice-add 'vertico--setup :after '~vertico-setup)
  (advice-add 'next-history-element :around '~vertico-scroll-up)
  (advice-add 'previous-history-element :around '~vertico-scroll-down)

  (advice-add 'vertico-repeat-last :around '~vertico-let-current-session)

  (advice-add 'read-file-name :around '~vertico-inhibit-repeat-save)
  (advice-add 'read-directory-name :around '~vertico-inhibit-repeat-save)
  (advice-add 'read-buffer :around '~vertico-inhibit-repeat-save))

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
  (let ((~vertico-current-session (or (car args)
                                      (car vertico-repeat-history))))
    (apply orig args)))

;; ミニバッファを扱う関数が実行されると、全て vertico-repeat の対象として保存されてしまうので、抑制できるようにしてる
(defun ~vertico-inhibit-repeat-save (orig &rest args)
  (let ((vertico-repeat-transformers (list '(lambda (x) nil))))
    (apply orig args)))


(bundle orderless)
(use-package orderless
  :custom ((orderless-component-separator 'orderless-escapable-split-on-space)
           (orderless-matching-styles '(orderless-regexp))
           (orderless-style-dispatchers '(~orderless-dispatcher-bang ~orderless-dispatcher-quote ~orderless-dispatcher-caret)))
  :init
  (setq completion-styles '(orderless basic))
  ;; (setq completion-category-defaults nil)
  (setq completion-category-overrides '((buffer (styles orderless basic))))
  (setq read-file-name-completion-ignore-case t)
  (setq read-buffer-completion-ignore-case t)
  (setq completion-ignore-case t)

  :config
  (defun ~orderless-dispatcher-bang (pattern _index _total)
    (cond
     ((equal "!" pattern)
      '(orderless-literal . ""))
     ((string-prefix-p "!" pattern)
      `(orderless-without-literal . ,(substring pattern 1)))))

  (defun ~orderless-dispatcher-quote (pattern _index _total)
    (cond
     ((equal "'" pattern)
      '(orderless-literal . ""))
     ((string-prefix-p "'" pattern)
      `(orderless-literal . ,(substring pattern 1)))))

  (defun ~orderless-dispatcher-caret (pattern _index _total)
    (cond
     ((equal "^" pattern)
      '(orderless-literal . ""))
     ((string-prefix-p "^" pattern)
      `(orderless-initialism . ,(substring pattern 1)))))
  )


(bundle marginalia)
(use-package marginalia
  :bind (:map minibuffer-local-map
              ("C-;" . marginalia-cycle))
  :init
  (marginalia-mode))

