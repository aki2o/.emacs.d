(bundle color-moccur)
(bundle moccur-edit)
(use-package color-moccur
  
  :bind* (("M-s l" . ~moccur-grep)
          ("M-s r" . ~moccur-grep-find)
          ("M-s L" . ~dmoccur)
          ("M-s R" . ~dmoccur-recursive)
          ("M-s b" . moccur))

  :commands (dired-do-moccur Buffer-menu-moccur)
  
  :init

  ;; カーソル移動だけで，該当箇所を別ウィンドウに表示
  (setq moccur-grep-following-mode-toggle t)

  ;; カーソル付近の単語をデフォルトの検索文字列とする
  (setq moccur-grep-default-word-near-point t)

  ;; 複数の検索語や、特定のフェイスのみマッチ等の機能を有効にする
  (setq moccur-split-word t)

  ;; migemoできる環境ならmigemoを使う
  (when (locate-library "migemo")
    (setq moccur-use-migemo t))

  ;; 検索対象から外すバッファ条件
  (setq *moccur-buffer-name-exclusion-list*
        '("^\\s*\\*" ;*で始まるバッファ
          "\\bE?TAGS\\b" ;TAGSファイル
          ))

  ;; キーワードによる検索をするかと、キーワードに対応する正規表現の定義
  ;; (setq moccur-use-keyword nil)
  ;; (setq moccur-search-keyword-alist
  ;;       '(("url"  . "[fht]*ttp://[-_.!~*'()a-zA-Z0-9;/?:@&=+$,%#]+")
  ;;         ("mail" . "[^][<>@ \n]+@[-_!~*'()a-zA-Z0-9?@&=+$,%#]+\\.[-_.!~*'()a-zA-Z0-9?@&=+$,%#]+")))

  ;; xdoc2txt連携関連
  ;; (setq moccur-use-xdoc2txt nil)
  ;; (setq moccur-grep-xdoc2txt-maximum-size 1000) ;検索対象にするファイルの上限サイズ (KB)
  ;; (setq moccur-grep-xdoc2txt-exts '("\\.rtf" "\\.doc" "\\.xls" "\\.ppt"
  ;;                                   "\\.jaw" "\\.jtw" "\\.jbw" "\\.juw"
  ;;                                   "\\.jfw" "\\.jvw" "\\.jtd" "\\.jtt"
  ;;                                   "\\.oas" "\\.oa2" "\\.oa3" "\\.bun"
  ;;                                   "\\.wj2" "\\.wj3" "\\.wk3" "\\.wk4"
  ;;                                   "\\.123" "\\.wri" "\\.pdf" "\\.mht"))

  ;; diredバッファでのバインド
  (define-key dired-mode-map (kbd "M") 'dired-do-moccur) ;マークしたファイル内検索

  (use-package ibuffer
    :defer t
    :config
    ;; ibufferバッファでのバインド
    ;; マークしたバッファ内検索
    (define-key ibuffer-mode-map (kbd "M") 'Buffer-menu-moccur))

  :config

  ;; dmoccur実行時にはバッファは増やさないようにする
  (defadvice dmoccur (around clean-buffer-dmoccur activate)
    (let* ((oldbuffs (buffer-list)))
      ad-do-it
      (loop for newb in (buffer-list)
            for found = nil
            do (loop for oldb in oldbuffs
                     if (eq newb oldb)
                     return (setq found t))
            if (and (not found)
                    (not (string-match "\\*Moccur\\*" (buffer-name newb))))
            do (kill-buffer newb))))

  ;; *Moccur*バッファで検索結果にジャンプするときには、開いていなければ開くようにする
  (defadvice moccur-get-info (before open-file-not-yet-opened activate)
    (when (and moccur-grep-following-mode-toggle
               moccur-view-other-window)
      (save-excursion
        (when (re-search-backward moccur-buffer-heading-regexp nil t)
          (let* ((file (match-string-no-properties 2)))
            (when (and (moccur-search-file-p file)
                       (not (get-file-buffer file)))
              (find-file-noselect file)))))))

  ;; *Moccur*バッファをリネームして残すようにする
  (defadvice moccur-setup (before rename-moccur-buffer-uniquely activate)
    (let* ((buff (get-buffer "*Moccur*")))
      (when (buffer-live-p buff)
        (save-excursion
          (with-current-buffer buff
            (rename-uniquely))))))

  ;; 色付けを変更
  (defadvice moccur-color-current-line (after remove-moccur-matched-text-overlay activate)
    (sit-for 0.5)
    (when moccur-current-line-overlays
      (delete-overlay moccur-current-line-overlays)
      (setq moccur-current-line-overlays nil)))
  
  (defadvice moccur-color-view (around nocolor-moccur-matched-text activate)
    (let* ((dummy))
      (when dummy ad-do-it)
      (setq ad-return-value nil)))
  
  (defadvice moccur-color-check-view (around nocheck-moccur-matched-text activate)
    (let* ((dummy))
      (when dummy ad-do-it)
      (setq ad-return-value nil)))
  
  (set-face-foreground 'moccur-face "white")
  (set-face-background 'moccur-face "cornflower blue")
  (set-face-bold-p 'moccur-face nil)
  (set-face-background 'moccur-current-line-face "blue")
  (set-face-underline-p 'moccur-current-line-face nil)

  ;; dmoccurをlgrep、rgrepのように使えるようにして、
  ;; dmoccurとmoccur-grepとの間のインタフェースを使いやすいように統一する
  (defun ~moccur-read-regexp ()
    (let* ((wd (or (when moccur-grep-default-word-near-point
                     (thing-at-point 'symbol))
                   "")))
      (when wd
        (set-text-properties 0 (length wd) nil wd))
      (read-string (format "Input Regexp List (%s): " wd) nil nil wd)))

  (defun ~moccur-read-filemask ()
    (let* ((wd (or moccur-grep-default-mask
                   ".*")))
      (read-string (format "Input FileMask (%s): " wd) nil nil wd)))

  (defun ~dmoccur (dir regexp mask arg)
    (interactive (list (dmoccur-read-from-minibuf current-prefix-arg)
                       (~moccur-read-regexp)
                       (~moccur-read-filemask)
                       current-prefix-arg))
    (setq dmoccur-mask (list mask))
    (setq dmoccur-recursive-search nil)
    (moccur-split-string regexp)
    (dmoccur dir regexp arg))

  (defun ~dmoccur-recursive (dir regexp mask arg)
    (interactive (list (dmoccur-read-from-minibuf current-prefix-arg)
                       (~moccur-read-regexp)
                       (~moccur-read-filemask)
                       current-prefix-arg))
    (setq dmoccur-mask (list mask))
    (setq dmoccur-recursive-search t)
    (moccur-split-string regexp)
    (dmoccur dir regexp arg))

  (defun ~moccur-grep (dir regexp mask)
    (interactive (list (moccur-grep-read-directory)
                       (~moccur-read-regexp)
                       (~moccur-read-filemask)))
    (moccur-grep dir (append (moccur-split-string regexp) (list mask))))

  (defun ~moccur-grep-find (dir regexp mask)
    (interactive (list (moccur-grep-read-directory)
                       (~moccur-read-regexp)
                       (~moccur-read-filemask)))
    (moccur-grep-find dir (append (moccur-split-string regexp) (list mask))))

  ;; moccurバッファでのキーバインド変更
  ;; moccurバッファが作られるたびにキーマップが再定義されてしまうので、
  ;; キーバインドを変更する関数を定義して、adviceで呼び出す
  (defun ~define-key-moccur-mode ()
    (local-set-key [(control j)] nil)
    (local-set-key [(control k)] nil)
    (local-set-key [(control h)] nil)
    (local-set-key [(control l)] nil)
    (local-set-key (kbd "j") 'moccur-next)
    (local-set-key (kbd "k") 'moccur-prev)
    (local-set-key (kbd "h") 'moccur-prev-file)
    (local-set-key (kbd "l") 'moccur-next-file)
    (local-set-key (kbd "J") 'moccur-scroll-down)
    (local-set-key (kbd "K") 'moccur-scroll-up))

  (defadvice moccur-mode (after redefine-moccur-mode-map activate)
    (~define-key-moccur-mode))
  
  (defadvice moccur-grep-mode (after redefine-moccur-mode-map activate)
    (~define-key-moccur-mode))

  (use-package moccur-edit)
  
  )


;; helm-c-moccur-buffer-listが動かなかったので、とりあえずanything-c-moccurを使っておく
;; (bundle helm-c-moccur)
;; (use-package helm-c-moccur

;;   :bind* (("M-s h o" . helm-c-moccur-occur-by-moccur)
;;           ("M-s h b" . helm-c-moccur-buffer-list)
;;           ("M-s h d" . helm-c-moccur-dmoccur)
;;           ("M-s h r" . helm-c-moccur-resume))

;;   :commands (helm-c-moccur-dired-do-moccur-by-moccur
;;              helm-c-moccur-from-isearch)
  
;;   :init
  
;;   (setq helm-c-moccur-push-mark-flag t)
;;   (setq helm-c-moccur-show-all-when-goto-line-flag t)
;;   (setq helm-c-moccur-higligt-info-line-flag t)
;;   (setq helm-c-moccur-enable-auto-look-flag t)
;;   (setq helm-c-moccur-enable-initial-pattern t)

;;   ;; diredバッファでのバインド
;;   (define-key dired-mode-map (kbd "M") 'helm-c-moccur-dired-do-moccur-by-moccur) ;diredでマークしたファイル内検索
  
;;   ;; isearch時のバインド
;;   (define-key isearch-mode-map (kbd "C-a") 'helm-c-moccur-from-isearch)
  
;;   :config

;;   (bind-keys :map helm-c-moccur-helm-map
;;               ("C-S-c d" . helm-c-moccur-wrap-symbol)
;;               ("C-S-c w" . helm-c-moccur-wrap-word)
;;               ("C-S-c f" . helm-c-moccur-match-only-function)
;;               ("C-S-c c" . helm-c-moccur-match-only-comment)
;;               ("C-S-c s" . helm-c-moccur-match-only-string)
;;               ("C-S-c i" . helm-c-moccur-match-only-internal)
;;               ("C-S-c h" . helm-c-moccur-start-symbol)
;;               ("C-S-c l" . helm-c-moccur-end-symbol)
;;               ("C-S-c n" . helm-c-moccur-start-word)
;;               ("C-S-c p" . helm-c-moccur-end-word))

;;   )


(bundle aki2o/anything-c-moccur)
(use-package anything-c-moccur
  
  :bind* (("M-s h o" . anything-c-moccur-occur-by-moccur)
          ("M-s h b" . anything-c-moccur-buffer-list)
          ("M-s h d" . anything-c-moccur-dmoccur)
          ("M-s h r" . anything-c-moccur-resume))

  :commands (anything-c-moccur-dired-do-moccur-by-moccur
             anything-c-moccur-from-isearch)
  
  :init
  
  (setq anything-c-moccur-anything-idle-delay 0.2)  ; `anything-idle-delay'
  (setq anything-c-moccur-higligt-info-line-flag t) ; `anything-c-moccur-dmoccur'などのコマンドでバッファの情報をハイライトする
  (setq anything-c-moccur-enable-auto-look-flag t)  ; 現在選択中の候補の位置を他のwindowに表示する
  (setq anything-c-moccur-enable-initial-pattern t) ; `anything-c-moccur-occur-by-moccur'の起動時にポイントの位置の単語を初期パターンにする

  ;; diredバッファでのバインド
  (define-key dired-mode-map (kbd "M") 'anything-c-moccur-dired-do-moccur-by-moccur) ;diredでマークしたファイル内検索
  
  ;; isearch時のバインド
  (define-key isearch-mode-map (kbd "C-a") 'anything-c-moccur-from-isearch)

  :config
  
  (bind-keys :map anything-c-moccur-anything-map
              ("C-j" . anything-c-moccur-next-line)
              ("C-k" . anything-c-moccur-previous-line)
              ("C-h" . anything-c-moccur-anything-previous-file-matches)
              ("C-l" . anything-c-moccur-anything-next-file-matches)
              ("C-r" . anything-c-moccur-query-replace-regexp)

              ("C-S-c d" . anything-c-moccur-wrap-symbol)
              ("C-S-c w" . anything-c-moccur-wrap-word)
              ("C-S-c f" . anything-c-moccur-match-only-function)
              ("C-S-c c" . anything-c-moccur-match-only-comment)
              ("C-S-c s" . anything-c-moccur-match-only-string)
              ("C-S-c i" . anything-c-moccur-match-only-internal)
              ("C-S-c h" . anything-c-moccur-start-symbol)
              ("C-S-c l" . anything-c-moccur-end-symbol)
              ("C-S-c n" . anything-c-moccur-start-word)
              ("C-S-c p" . anything-c-moccur-end-word))

  )

