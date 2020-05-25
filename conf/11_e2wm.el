(use-package e2wm
  :defer t
  :init
  (use-package deferred :defer t)

  (unbind-key "M-m")
  (setq e2wm:prefix-key "M-m ")

  (custom-set-faces
   '(e2wm:face-subtitle ((((class color) (background light))
                          (:foreground "Gray10" :height 0.8 :inherit variable-pitch))
                         (((class color) (background dark))
                          (:foreground "Gray90" :height 0.8 :inherit variable-pitch))
                         (t
                          :height 0.8 :inherit variable-pitch)))
   '(e2wm:face-item ((t :height 0.8 :inherit variable-pitch :foreground "DarkSlateBlue")))
   '(e2wm:face-history-list-normal ((t :foreground "ivory"))))

  :config
  ;; e2wm:add-keymap がエラーになるので、一旦コメントアウト
  ;; (use-package e2wm-config)

  (setq e2wm:c-max-history-num 100) ; 履歴の保存数

  ;; ドキュメント的に扱いたいバッファ
  (setq ~e2wm:regexp-doc-buff
        (rx-to-string `(and bos "*" (or "Help" "info" "eww" "w3m" "Woman " "Man "
                                        "perldoc " "plsense help" "~lsp-ui-doc "))))
  (setq e2wm:c-document-buffer-p
        (lambda (buf)
          (string-match ~e2wm:regexp-doc-buff (buffer-name buf))))

  ;; バッファ履歴にドキュメントも含める
  (setq e2wm:c-recordable-buffer-p
        (lambda (buf)
          (or (buffer-local-value 'buffer-file-name buf)
              (string-match ~e2wm:regexp-doc-buff (buffer-name buf)))))
  

  ;; 既存パースペクティブ定義カスタマイズ

  ;; code
  (setq e2wm:c-code-recipe
        '(| (:left-max-size 40)
            (- (:upper-size-ratio 0.7)
               tree history)
            (- (:lower-max-size 150)
               (| (:right-max-size 45)
                  main
                  (- sww outline))
               sub)))

  (setq e2wm:c-code-winfo
        '((:name main)
          (:name tree    :plugin direx)
          (:name history :plugin perspb)
          (:name outline :plugin files :sww sww)
          (:name outline :plugin imenu :sww sww :sww-label "IMenu" :sww-default t)
          (:name outline :plugin pkgex4pl :sww sww :sww-label "PlTree")
          (:name sww     :plugin sww)
          (:name sub     :buffer "*info*" :default-hide t)))

  ;; array
  (setq e2wm:c-array-font-decrease 2)
  (setq e2wm:c-array-smart-buffers-functions
        '(e2wm:dp-array-get-same-mode-buffers-if-not-recordable))
  (setq e2wm:c-array-summary-size-ratio 0.01)

  ;; pgkex4pl
  (setq e2wm-pkgex4pl:sync-interval 2)
  (add-hook 'cperl-mode-hook
            '(lambda () (add-to-list 'e2wm-sww:default-plugins 'pkgex4pl)) t)

  
  ;; 挙動変更
  (defvar ~e2wm:delay-handled-buffer nil)

  (defadvice e2wm:pst-change (around ~e2wm:fix-prev-selected-buffer activate)
    (setq ~e2wm:delay-handled-buffer
          (when (and (memq (ad-get-arg 0) '(code two htwo svn git))
                     (not (e2wm:history-recordable-p (current-buffer))))
            (current-buffer)))
    ad-do-it
    (when ~e2wm:delay-handled-buffer
      (e2wm:pst-method-call e2wm:$pst-class-switch
                            (e2wm:pst-get-instance)
                            ~e2wm:delay-handled-buffer)))

  (defadvice e2wm:dp-code-init (before ~e2wm:disable-prev-selected-buffer activate)
    (when ~e2wm:delay-handled-buffer
      (setq e2wm:prev-selected-buffer nil)))

  (defadvice e2wm:dp-two-init (before ~e2wm:disable-prev-selected-buffer activate)
    (when ~e2wm:delay-handled-buffer
      (setq e2wm:prev-selected-buffer nil)))

  (defadvice e2wm:dp-htwo-init (before ~e2wm:disable-prev-selected-buffer activate)
    (when ~e2wm:delay-handled-buffer
      (setq e2wm:prev-selected-buffer nil)))

  
  ;; e2wm有効な場合に画面更新がされない機能のための対処
  (defmacro ~e2wm:window-update-ize (command)
    (declare (indent 0))
    `(defadvice ,command (after ~e2wm:window-update activate)
       (when (e2wm:managed-p)
         (dolist (wnd (window-list))
           (let ((pt (with-current-buffer (window-buffer wnd)
                       (point))))
             (when (not (= (window-point wnd) pt))
               (set-window-point wnd pt)))))))

  (~e2wm:window-update-ize find-function)
  (~e2wm:window-update-ize find-variable)
  (~e2wm:window-update-ize pop-tag-mark)
  (~e2wm:window-update-ize helm-ag--find-file-action)
  (~e2wm:window-update-ize helm-git-grep-persistent-action)
  (~e2wm:window-update-ize ivy-call)
  (~e2wm:window-update-ize robe-jump)
  (~e2wm:window-update-ize godef-jump)


  ;; タグジャンプとかでハイライトさせたい
  (defvar ~e2wm:highlight-current-line-overlay nil)
  
  (defmacro ~e2wm:highlight-current-line-after (command wname)
    (declare (indent 0))
    `(defadvice ,command (after ~e2wm:highlight-current-line activate)
       (when (e2wm:managed-p)
         (e2wm:aif (wlf:get-window (e2wm:pst-get-wm) ',wname)
             (with-selected-window it
               (let ((start (point-at-bol))
                     (end (1+ (point-at-eol))))
                 (if (not ~e2wm:highlight-current-line-overlay)
                     (setq ~e2wm:highlight-current-line-overlay (make-overlay start end))
                   (move-overlay ~e2wm:highlight-current-line-overlay start end))
                 (overlay-put ~e2wm:highlight-current-line-overlay 'face 'highlight)
                 (run-with-idle-timer
                  0.5
                  nil
                  '(lambda ()
                     (when ~e2wm:highlight-current-line-overlay
                       (delete-overlay ~e2wm:highlight-current-line-overlay)
                       (setq ~e2wm:highlight-current-line-overlay nil))))))))))

  (~e2wm:highlight-current-line-after find-function right)
  (~e2wm:highlight-current-line-after find-variable right)
  (~e2wm:highlight-current-line-after helm-ag--action-find-file right)
  (~e2wm:highlight-current-line-after robe-jump right)
  (~e2wm:highlight-current-line-after godef-jump right)

  
  ;; コマンド
  (defun ~e2wm:restart-management (&optional pstset)
    (interactive)
    (e2wm:start-management pstset t))

  (defun ~e2wm:delete-window (&optional not-focus-main)
    (interactive)
    (let* ((wm (e2wm:pst-get-wm))
           ;; (active-wname (wlf:get-window-name wm (selected-window))))
           )
      (loop for wname in '(sub diff tree)
            ;; for winfo = (wlf:get-winfo wname (wlf:wset-winfo-list wm))
            ;; for defhide = (wlf:window-option-get winfo :default-hide)
            if (ignore-errors (window-live-p (wlf:get-window wm wname)))
            return (progn (wlf:hide wm wname)
                          (when (not not-focus-main)
                            (e2wm:pst-window-select-main))))))

  (defun ~e2wm:sub-maximize-toggle-command ()
    (interactive)
    (wlf:toggle-maximize (e2wm:pst-get-wm) 'sub))


  ;; キーバインド
  (e2wm:add-keymap
   e2wm:pst-minor-mode-keymap
   '(("prefix R" . ~e2wm:restart-management)
     ("prefix L" . ielm)
     ("prefix I" . info)
     ("prefix m" . e2wm:pst-window-select-main-command)
     ("prefix s" . ~e2wm:sub-maximize-toggle-command)
     ("prefix z" . ~e2wm:delete-window)
     ("prefix n" . nil)
     ("prefix p" . nil)
     ("C-}"      . e2wm:pst-history-forward-command) ; 履歴を進む
     ("C-{"      . e2wm:pst-history-back-command) ; 履歴をもどる
     ("C-z"      . ~e2wm:delete-window)
     ) e2wm:prefix-key)

  (e2wm:add-keymap
   e2wm:dp-code-minor-mode-map
   '(;; ("M-i" . e2wm:dp-code-imenu-toggle-command)
     ) e2wm:prefix-key)

  (e2wm:add-keymap
   e2wm:dp-array-minor-mode-map
   '(("C-j" . e2wm:dp-array-move-down-command)
     ("C-k" . e2wm:dp-array-move-up-command)
     ("C-h" . e2wm:dp-array-move-left-command)
     ("C-l" . e2wm:dp-array-move-right-command)
     ) e2wm:prefix-key)
  )


(use-package e2wm-sww
  :defer t
  :after (e2wm))


(use-package e2wm-transcribe
  :straight (:type built-in)
  :defer t
  :config
  (setq e2wm:c-transcribe-recipe
        '(- (:upper-size-ratio 0.55)
            (| (:left-size-ratio 0.45)
               (| (:left-size-ratio 0.5)
                  tree left)
               (| (:right-size-ratio 0.34)
                  right history))
            sub))

  (setq e2wm:c-transcribe-winfo
        '((:name left)
          (:name right)
          (:name history :plugin perspb)
          (:name tree    :plugin direx :default-hide t)
          (:name sub     :default-hide t)))

  (defun ~e2wm-transcribe:history-toggle-command ()
    (interactive)
    (wlf:toggle (e2wm:pst-get-wm) 'history)
    (e2wm:pst-update-windows))

  (defun ~e2wm-transcribe:tree-show-command ()
    (interactive)
    (let ((wm (e2wm:pst-get-wm)))
      (wlf:show wm 'tree)
      (wlf:select wm 'tree)
      (e2wm:pst-update-windows)))

  (e2wm:add-keymap
   e2wm:dp-transcribe-minor-mode-map
   '(("prefix j" . e2wm:dp-two-swap-buffers-command)
     ("prefix M" . e2wm:dp-two-main-maximize-toggle-command)
     ("prefix d" . ~e2wm-transcribe:tree-show-command)
     ("prefix i" . ~e2wm-transcribe:history-toggle-command)
     ("C-}"      . e2wm-perspb:switch-to-down-entry-command)
     ("C-{"      . e2wm-perspb:switch-to-up-entry-command)
     ) e2wm:prefix-key)

  (defadvice direx:find-item (around ~e2wm-transcribe:close-tree activate)
    (let ((wnd (selected-window)))
      ad-do-it
      (when (and (window-live-p wnd)
                 (e2wm:managed-p)
                 (eq (e2wm:$pst-name (e2wm:pst-get-instance)) 'transcribe))
        (delete-window wnd))))
  
  (defadvice direx:find-item-other-window (around ~e2wm-transcribe:close-tree activate)
    (let ((wnd (selected-window)))
      ad-do-it
      (when (and (window-live-p wnd)
                 (e2wm:managed-p)
                 (eq (e2wm:$pst-name (e2wm:pst-get-instance)) 'transcribe))
        (delete-window wnd)))))


(use-package e2wm-term
  :defer t
  :config
  (setq e2wm-term:default-backend 'shell)
  (setq e2wm-term:help-window-default-hide t)
  (setq e2wm-term:help-guess-command t)
  (add-hook 'e2wm-term:input-mode-hook '~keyjack-mode t)
  (add-to-list 'e2wm-term:shell-password-prompt-regexps "^Vault +password: +")
  (add-to-list 'e2wm-term:shell-password-prompt-regexps "^Confirm +Vault +password: +")
  (add-to-list 'e2wm-term:shell-password-prompt-regexps "^SUDO +password: +")
  (add-to-list 'e2wm-term:shell-password-prompt-regexps "^SSH +password: +"))
