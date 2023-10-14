(bundle consult :type git :url "git@github.com:aki2o/consult.git" :branch "fix_jump_preview")
(use-package consult
  :custom ((consult-narrow-key (kbd "C-n"))
           (consult-preview-key (kbd "C-M-;"))
           (consult-preview-raw-size 1048576)
           (consult-async-min-input 5))

  :bind (([remap apropos-command]    . consult-apropos)
         ([remap pop-global-mark]    . consult-global-mark)
         ([remap recentf-open-files] . consult-recent-file)
         ([remap yank-pop]           . consult-yank-from-kill-ring)
         :map ~keyjack-mode-map
         ([remap pop-global-mark]    . consult-global-mark)
         ([remap recentf-open-files] . consult-recent-file)
         ([remap yank-pop]           . consult-yank-from-kill-ring)
         :map isearch-mode-map
         ("C-M-p" . consult-isearch-history)
         ("M-c" . consult-line)
         :map minibuffer-local-map
         ([remap next-matching-history-element]     . consult-history)
         ([remap previous-matching-history-element] . consult-history))

  :hook (completion-list-mode . consult-preview-at-point-mode)

  :init
  (setq register-preview-function 'consult-register-format)
  (advice-add 'register-preview :override 'consult-register-window)

  (setq xref-show-xrefs-function 'consult-xref)
  (setq xref-show-definitions-function 'consult-xref)

  :config
  (define-key consult-narrow-map (vconcat consult-narrow-key "?") 'consult-narrow-help)

  (setq consult-after-jump-hook '(~pulse-momentary))

  (advice-add 'consult-yank-from-kill-ring :after '~consult-update-current-kill-to)

  (with-eval-after-load 'pophint-autoloads
    (pophint-thing:defcommand-noadvice ~consult-grep)
    (pophint-thing:defcommand-noadvice ~consult-git-grep)
    (pophint-thing:defcommand-noadvice ~consult-ripgrep))
  )

(defun ~consult-update-current-kill-to (string &optional arg)
  (let ((curr (current-kill 0 t)))
    (when (not (string= curr string))
      (current-kill (- (seq-position kill-ring string)
                       (seq-position kill-ring curr))))))

;; vertico-repeat は、最初のミニバッファの入力を変えるらしく、
;; 最初にディレクトリを選択するコマンドでは、 vertico-repeat を使えないので、
;; vertico-repeat-transformers をいじって、別のコマンドが実行されるようにしている
(defun ~consult-resume ()
  (interactive)
  (let* ((session ~vertico-current-session)
         (v (funcall orderless-component-separator (cadr session)))
         (c (pop v))
         (f (intern-soft (format "consult-%s" c)))
         (dir (pop v))
         (input (mapconcat 'identity v " "))
         (vertico-repeat-transformers `(,@vertico-repeat-transformers ~consult-resume-transformer))
         (~consult-resumed-command c)
         (~consult-resumed-directory dir))
    (when (not (functionp f))
      (error "Invalid session : %s" session))
    ;; 本来の入力値とコマンドで実行
    (setf (nth 1 session) input)
    (unwind-protect
        (funcall f dir input)
      (setf (nth 1 session) (mapconcat 'identity (list c dir input) " ")))))

(defvar ~consult-resumed-command nil)
(defvar ~consult-resumed-directory nil)

(defun ~consult-resume-transformer (session)
  (when session
    (list '~consult-resume
          (mapconcat 'identity (list ~consult-resumed-command ~consult-resumed-directory (cadr session)) " ")
          (caddr session))))

(defun ~consult-grep (dir)
  (interactive
   (list (read-directory-name "Dir: ")))
  (let* ((input (~dwim-thing-at-point))
         (vertico-repeat-transformers `(,@vertico-repeat-transformers ~consult-resume-transformer))
         (~consult-resumed-command "grep")
         (~consult-resumed-directory dir))
    (funcall 'consult-grep dir input)))

(defun ~consult-git-grep (&optional dir)
  (interactive)
  (let* ((input (~dwim-thing-at-point))
         (vertico-repeat-transformers `(,@vertico-repeat-transformers ~consult-resume-transformer))
         (~consult-resumed-command "git-grep")
         (~consult-resumed-directory dir))
    (funcall 'consult-git-grep dir input)))

(defun ~consult-ripgrep (dir)
  (interactive
   (list (read-directory-name "Dir: ")))
  (let* ((input (~dwim-thing-at-point))
         (vertico-repeat-transformers `(,@vertico-repeat-transformers ~consult-resume-transformer))
         (~consult-resumed-command "ripgrep")
         (~consult-resumed-directory dir))
    (funcall 'consult-ripgrep dir input)))

