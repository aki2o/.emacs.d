;; 様子見
;; (setq confirm-nonexistent-file-or-buffer nil)


;; ;; For remote
;; (defun ~rename-buffer-for-tramp ()
;;   (when (file-remote-p (buffer-file-name))
;;     (rename-buffer (format "%s:%s"
;;                            (file-remote-p (buffer-file-name) 'method)
;;                            (buffer-name)))))
;; (add-hook 'find-file-hook '~rename-buffer-for-tramp t)


;; (bundle emacswiki:sudo-ext)
;; (when (server-running-p)
;;   (require 'sudo-ext)
;;   )


(defvar ~tramp-path-regexp "\\`/\\([a-z]+\\):")

;; 毎回聞かれるのはうざかったので、最初は普通に開く
;; 
;; ;; 書き込めないファイルならsudoするか聞く
;; (defun ~open-as-root-p (file)
;;   (and file
;;        (file-exists-p file)
;;        (file-regular-p file)
;;        (not (file-writable-p file))
;;        (y-or-n-p (format "'%s' is read-only. Open it as root? " file))))
;; 
;; (defadvice find-file (before ~open-as-root activate)
;;   (let* ((file (ad-get-arg 0))
;;          (tramp-path (when (~open-as-root-p file)
;;                        (if (string-match ~tramp-path-regexp file)
;;                            (replace-regexp-in-string ~tramp-path-regexp "/sudo:" file)
;;                          (concat "/sudo::" file)))))
;;     (when tramp-path
;;       (ad-set-arg 0 tramp-path))))

(use-package tramp
  :defer t
  :config
  (setq tramp-default-method "scpx")
  
  ;; sshの実行にf_sshを含める
  (setcdr (assq 'tramp-login-program (assoc "ssh" tramp-methods)) '("f_ssh" "ssh"))

  ;; 多段接続
  ;; Ex.) localhost -> ssh -> foo@host1 -> ssh -> bar@host2
  ;; (add-to-list 'tramp-default-proxies-alist '("host2" "bar" "/ssh:foo@host1"))
  
  ;; リモートをrootで操作する時、
  ;; まずssh_configで設定されたユーザまたはローカルユーザで接続する
  (add-to-list 'tramp-default-proxies-alist '(nil "\\`root\\'" "/ssh:%h:"))
  ;; 但し、直接rootログインできるホストやローカルホストは除く
  (dolist (h `("localhost"
               ,(regexp-quote (system-name))))
    (add-to-list 'tramp-default-proxies-alist `(,h nil nil)))
  
  ;; For avoiding freez
  (defadvice tramp-handle-vc-registered (around ~fix-handled-backends activate)
    (let ((vc-handled-backends '(SVN Git)))
      ad-do-it))

  ;; For avoiding use of tramp format buffer-file-name
  (defmacro ~tramp-use-original-buffer-file-name-in (func)
    `(defadvice ,func (around tramp-use-original-buffer-file-name activate)
       (let* ((f buffer-file-name)
              (buffer-file-name (if (tramp-tramp-file-p f)
                                    (tramp-file-name-localname (tramp-dissect-file-name f))
                                  f)))
         ad-do-it)))
  
  ;; (~tramp-use-original-buffer-file-name-in shell)

  ;; 2016-05-16
  ;; trampが最初にauth-source-searchを呼び出すと固まってしまうため、その前に手動実行する用
  (defun ~tramp-just-do-auth-source-search ()
    (interactive)
    (auth-source-search :user t :host "dummy" :port "dummy")
    (message "Done!")))

