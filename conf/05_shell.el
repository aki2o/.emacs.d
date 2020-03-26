;; カラーシーケンスなどの制御コードは除去する
(defun ~shell-command-output-without-untext-code (s)
  (let* ((s (replace-regexp-in-string "\r" "" s))
         (s (replace-regexp-in-string "\x1B\\[\\([0-9][0-9]?\\(;[0-9][0-9]?\\)?\\)?[m|K]" "" s)))
    s))

(defadvice shell-command (after ~drop-untext-code activate)
  (let ((buf (or (ad-get-arg 1) (get-buffer "*Shell Command Output*"))))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (let ((s (~shell-command-output-without-untext-code (buffer-string))))
          (erase-buffer)
          (insert s))))))

(defadvice shell-command-to-string (after ~drop-untext-code activate)
  (setq ad-return-value (~shell-command-output-without-untext-code ad-return-value)))


(bundle readline-complete)
(bundle company-shell)
(bundle shell-command)
(bundle shell-history)
(use-package shell
  :defer t

  :config

  (setq shell-file-name (or (executable-find "bash")
                            (executable-find "sh")))
  (setq explicit-shell-file-name (or (executable-find "f_bash")
                                     (executable-find "bash")
                                     (executable-find "sh")))
  (setq shell-command-switch "-c")

  ;; 環境変数
  (setenv "SHELL" shell-file-name)

  (add-hook 'shell-mode-hook '~shell-mode-setup t)
  (defun ~shell-mode-setup ()
    (comint-read-input-ring t)
    ;; エスケープシーケンスで色付け
    (setq ansi-color-names-vector
          ["#000000"           ; black
           "#ff6565"           ; red
           "#93d44f"           ; green
           "#eab93d"           ; yellow
           "#204a87"           ; blue
           "#ce5c00"           ; magenta
           "#89b6e2"           ; cyan
           "#ffffff"]          ; white
          )
    (ansi-color-for-comint-mode-on)
    ;; 文字コード
    ;; Cygwin環境では環境変数LANGに合わせるっぽ。
    (cond ((~is-windows) (set-buffer-process-coding-system 'utf-8-dos 'utf-8-unix))
          ((~is-mac)     (set-buffer-process-coding-system 'utf-8-nfd-unix 'utf-8-nfd-unix))
          (t             (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix)))
    (pcomplete-shell-setup))

  ;; パスワード入力時はミニバッファで伏字表示
  (add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt nil)
  
  ;; Rename
  (defadvice shell (after rename-uniquely activate)
    (rename-uniquely))

  ;; ;; NTEmacsの場合には以下をしないと文字化ける
  ;; (when (eq system-type 'windows-nt)
  ;;   (defadvice shell-command (around shell-command-ad-set-coding activate)
  ;;     (let ((coding-system-for-read 'utf-8-dos) (coding-system-for-write 'sjis-dos)) ad-do-it))
  ;;   (defadvice shell-command-on-region (around shell-command-on-region-ad-set-coding activate)
  ;;     (let ((coding-system-for-read 'utf-8-dos) (coding-system-for-write 'sjis-dos)) ad-do-it))
  ;;   (defadvice shell-command-to-string (around shell-command-to-string-ad-set-coding activate)
  ;;     (let ((coding-system-for-read 'utf-8-dos) (coding-system-for-write 'sjis-dos)) ad-do-it))
  ;;   (defadvice compile (around compile-ad-set-coding activate)
  ;;     (let ((coding-system-for-read 'utf-8-dos) (coding-system-for-write 'sjis-dos)) ad-do-it))
  ;;   (defadvice executable-interpret (around executable-interpret-ad-set-coding activate)
  ;;     (let ((coding-system-for-read 'utf-8-dos) (coding-system-for-write 'sjis-dos)) ad-do-it))
  ;;   (defadvice start-process-shell-command (around start-process-shell-command-ad-set-coding activate)
  ;;     (let ((coding-system-for-read 'utf-8-dos) (coding-system-for-write 'sjis-dos)) ad-do-it))
  ;;   )

  ;; ;; シェル実行の前にrcファイルを読み込ませる
  ;; (defun ~source-available-rcfile (cmdstr)
  ;;   (let* ((shellnm (cond ((stringp shell-file-name) (replace-regexp-in-string "\\.exe$" "" (file-name-nondirectory shell-file-name)))
  ;;                         (t                         "")))
  ;;          (rcnm (cond ((string= shellnm "bash") ".bashrc")
  ;;                      ((string= shellnm "f_bash") ".bashrc")
  ;;                      ((string= shellnm "tcsh") ".cshrc")
  ;;                      ((string= shellnm "f_tcsh") ".cshrc")
  ;;                      ((string= shellnm "sh") ".shrc"))))
  ;;     (cond ((and (stringp rcnm)
  ;;                 (file-exists-p (concat "~/" rcnm)))
  ;;            (concat "source ~/" rcnm "; " cmdstr))
  ;;           (t
  ;;            cmdstr))))
  ;; (defadvice shell-command (before shell-command-ad-source-rc activate)
  ;;   (ad-set-arg 0 (~source-available-rcfile (ad-get-arg 0))))
  ;; (defadvice shell-command-on-region (before shell-command-on-region-ad-source-rc activate)
  ;;   (ad-set-arg 2 (~source-available-rcfile (ad-get-arg 2))))
  ;; (defadvice shell-command-to-string (before shell-command-to-string-ad-source-rc activate)
  ;;   (ad-set-arg 0 (~source-available-rcfile (ad-get-arg 0))))
  ;; (defadvice compile (before compile-ad-source-rc activate)
  ;;   (ad-set-arg 0 (~source-available-rcfile (ad-get-arg 0))))
  ;; (defadvice executable-interpret (before executable-interpret-ad-source-rc activate)
  ;;   (ad-set-arg 0 (~source-available-rcfile (ad-get-arg 0))))
  ;; (defadvice start-process-shell-command (before start-process-shell-command-ad-source-rc activate)
  ;;   (ad-set-arg 2 (~source-available-rcfile (ad-get-arg 2))))

  
  (use-package comint
    :config
    (bind-keys :map comint-mode-map
               ("C-c C-k" . comint-previous-input)
               ("C-c C-j" . comint-next-input)
               ("C-M-p"   . comint-previous-matching-input-from-input)
               ("C-M-n"   . comint-next-matching-input-from-input)))

  
  (use-package readline-complete
    :config
    (setq explicit-bash-args '("-c" "export EMACS=; stty echo; bash"))
    (setq comint-process-echoes t)

    (defun ~readline-complete-setup ()
      (add-to-list 'ac-sources 'ac-source-shell))
    (add-hook 'shell-mode-hook '~readline-complete-setup t))

  (use-package shell-history)
  
  )


(use-package ansi-color
  :commands ansi-color-for-comint-mode-on)


(bundle emacswiki:background)
(use-package background
  :commands (background))


;; (bundle multi-term)
;; (bundle multi-shell)


(defun ~shell-command-on-region-each-line (start end command-format)
  (interactive (let (string)
                 (unless (mark)
                   (error "The mark is not set now, so there is no region"))
                 (setq string (read-shell-command "Shell command format using %s: "))
                 (list (region-beginning) (region-end) string)))
  (dolist (line (split-string (buffer-substring-no-properties start end) "\n"))
    (let* ((arg (s-trim line))
           (cmd (format command-format arg)))
      (when (not (string= arg ""))
        (message "Invoke shell command : %s" cmd)
        (shell-command cmd)))))

(defun ~shell-command-on-region-with-replace (start end command)
  (interactive (let (string)
                 (unless (mark)
                   (error "The mark is not set now, so there is no region"))
                 (setq string (read-shell-command "Shell command: "))
                 (list (region-beginning) (region-end) string)))
  (shell-command-on-region start end command nil t))

(defun ~shell-command-insert-result (command)
  (interactive (let (string)
                 (setq string (read-shell-command "Shell command: "))
                 (list string)))
  (insert (shell-command-to-string command)))
