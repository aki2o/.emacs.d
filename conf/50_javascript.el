(use-package rjsx-mode
  :defer t
  :init
  (add-to-list 'auto-mode-alist '(".*\\.js\\'" . rjsx-mode))

  :config
  (add-hook 'rjsx-mode-hook '~rjsx-mode-setup t)
  (defun ~rjsx-mode-setup ()
    (setq indent-tabs-mode nil)
    (setq js-indent-level 2)
    (setq js2-strict-missing-semi-warning nil) ;;行末のセミコロンの警告はオフ
    )
  )

(use-package js2-mode
  :defer t
  :init
  (mmask-regist-extension-with-icase 'js2-mode "js" "jse" "gs" "js.erb")

  :config
  (add-hook 'js2-mode-hook '~js2-mode-setup t)
  (defun ~js2-mode-setup ()
    (setq js-indent-level 2)
    ;; color-moccur
    (setq moccur-grep-default-mask (mmask-get-regexp-string 'js2-mode)))

  ;; flymake
  ;; (defun flymake-js16-init ()
  ;;   (let* ((temp-file (flymake-init-create-temp-buffer-copy 'flymake-create-temp-inplace))
  ;;          (local-file (file-relative-name temp-file (file-name-directory buffer-file-name))))
  ;;     (list "~/app/spidermonkey-1.6/js" (list "-sC" local-file))))
  ;; (defun flymake-js17-init ()
  ;;   (let* ((temp-file (flymake-init-create-temp-buffer-copy 'flymake-create-temp-inplace))
  ;;          (local-file (file-relative-name temp-file (file-name-directory buffer-file-name))))
  ;;     (list "js" (list "-sC" local-file))))

  ;; ;; .js書くときはJavaScript 1.6相当。user.js書くときは1.7相当でsyntaxチェック
  ;; (push (list (mmask-get-regexp-string 'js-mode) 'flymake-js16-init) flymake-allowed-file-name-masks)
  ;; (push '("/user\\.js\\'" flymake-js17-init) flymake-allowed-file-name-masks)

  ;; (defun flymake-js-load ()
  ;;   (interactive)
  ;;   (setq flymake-err-line-patterns '(("^\\(.+\\)\:\\([0-9]+\\)\: \\(SyntaxError\:.+\\)\:$" 1 2 nil 3)))
  ;;   (append flymake-err-line-patterns '(("^\\(.+\\)\:\\([0-9]+\\)\: \\(strict warning: trailing comma.+\\)\:$" 1 2 nil 3)))
  ;;   (local-set-key (kbd "C-b") 'flymake-goto-next-error-and-popup)
  ;;   (local-set-key (kbd "C-S-b") 'flymake-goto-prev-error-and-popup)
  ;;   (flymake-mode t))

  ;; (add-hook 'js2-mode-hook 'flymake-js-load t)
  )


(use-package json-mode
  :defer t
  :init
  (mmask-regist-extension-with-icase 'json-mode "json")
  (mmask-regist-name 'json-mode ".tern-project")

  :config
  (defun ~json-setup-mode ()
    (setq js-indent-level 2))
  (add-hook 'json-mode-hook '~json-setup-mode t))


(use-package tern
  :defer t
  :commands (tern-mode)
  :init
  (dolist (h '(rjsx-mode-hook js2-mode-hook nxml-mode-hook web-mode-hook))
    (add-hook h '(lambda () (tern-mode 1)) t))

  :config
  (bind-keys :map tern-mode-keymap
             ("C-'"   . tern-get-docs)
             ("C->"   . tern-find-definition)
             ("M-."   . nil)
             ("M-RET" . tern-ac-complete))
  
  (~tags-be-find-tag-command tern-show-definition)

  ;; (add-to-list 'tern-command "--no-port-file" t)

  ;; (~tramp-use-original-buffer-file-name-in tern-get-type)
  ;; (~tramp-use-original-buffer-file-name-in tern-get-docs)
  ;; (~tramp-use-original-buffer-file-name-in tern-find-definition)
  ;; (~tramp-use-original-buffer-file-name-in tern-find-definition-by-name)
  ;; (~tramp-use-original-buffer-file-name-in tern-pop-find-definition)

  (when (fboundp 'pophint-tags:advice-command)
    (pophint-tags:advice-command tern-find-definition)))


;; エラーになってしまったのでコメントアウト
;; (use-package company-tern
;;   :after (tern))


;; (use-package tern-auto-complete
;;   :after (tern)
;;   :config
;;   (tern-ac-setup))
  

;; For debug p-r

(eval-after-load 'tern-auto-complete
  '(progn

(defun tern-ac-complete-request (cc)
  (setq tern-last-point-pos (point))
  (setq tern-ac-complete-reply nil)
  (setq tern-ac-complete-request-point (point))
  (tern-run-query 
   (lambda (data) 
     (tern-ac-complete-response data)
     (funcall cc))
   `((type . "completions") (types . t) (docs . t) (caseInsensitive . t))
   (point)))

;; (defun tern-ac-complete ()
;;   "Complete code at point by tern."
;;   (interactive)
;;   (tern-ac-complete-request
;;    (lambda ()
;;      (let ((ac-sources ac-sources))
;;        (add-to-list 'ac-sources 'ac-source-tern-completion)
;;        (ac-start)))))

))
