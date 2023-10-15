;; 様子見
;; (setq confirm-nonexistent-file-or-buffer nil)

(with-eval-after-load 'tramp
  (setq tramp-default-method "scpx")

  ;; 多段接続
  ;; Ex.) localhost -> ssh -> foo@host1 -> ssh -> bar@host2
  ;; (add-to-list 'tramp-default-proxies-alist '("host2" "bar" "/ssh:foo@host1"))
  ;;
  ;; リモートをrootで操作する時、まずssh_configで設定されたユーザまたはローカルユーザで接続する
  (add-to-list 'tramp-default-proxies-alist '(nil "\\`root\\'" "/ssh:%h:"))
  ;; 但し、直接rootログインできるホストやローカルホストは除く
  (dolist (h `("localhost" ,(regexp-quote (system-name))))
    (add-to-list 'tramp-default-proxies-alist `(,h nil nil)))

  ;; trampが最初にauth-source-searchを呼び出すと固まってしまうため、その前に実行しておく
  (with-eval-after-load 'auth-source
    (auth-source-search :user t :host "dummy" :port "dummy"))
  )
