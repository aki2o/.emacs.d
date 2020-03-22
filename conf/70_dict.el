(use-package sdic

  :bind* (("M-t" . ~sdic-smart-describe))

  :init
  
  (setq sdic-window-height 10)
  (setq sdic-disable-select-window t)
  (setq sdic-eiwa-dictionary-list '((sdicf-client (concat user-emacs-directory "sdic/gene.sdic"))))
  (setq sdic-waei-dictionary-list '((sdicf-client (concat user-emacs-directory "sdic/jedict.sdic") (add-keys-to-headword t))))

  :config
  
  (defadvice sdic-display-buffer (around custom-display activate)
    (cond ((featurep 'popwin)
           (funcall display-buffer-function (current-buffer)))
          (t
           ad-do-it)))

  (defun ~sdic-smart-describe ()
    (interactive)
    (cond ((and transient-mark-mode mark-active)
           (sdic-describe-region (region-beginning) (region-end)))
          (t
           (sdic-describe-word-at-point))))

  )


(bundle emacswiki:sdic-inline)
(use-package sdic-inline

  :bind* (("M-t" . ~sdic-inline-smart-display))
  
  :config
  
  (setq sdic-inline-eiwa-dictionary (concat user-emacs-directory "sdic/gene.sdic"))
  (setq sdic-inline-waei-dictionary (concat user-emacs-directory "sdic/jedict.sdic"))
  ;; (setq sdic-inline-dictionary-encoding 'euc-jp)
  (setq sdic-inline-search-func 'sdic-inline-search-word-with-stem) ;過去形などを自動的に判別し適切な語で検索
  (setq sdic-inline-word-at-point-strict t)                         ;空白上にポイントしていたら発動しない
  (sdic-inline-mode 0)                    ;自動実行するかどうか
  ;; ;; 自動実行する条件
  ;; (setq sdic-inline-enable-modes nil)
  ;; (setq sdic-inline-enable-faces nil)
  ;; (setq sdic-inline-enable-filename-regex ".*")
  ;; ;; 自動実行時の挙動
  ;; (setq sdic-inline-delay 2)
  ;; (setq sdic-inline-not-search-style 'point) ; デフォルト値。ポイント位置が前回と同じである限り、再度辞書ではひかない。
  ;; (setq sdic-inline-not-search-style 'word)  ; カーソル下の単語が前回辞書で引いた単語と同じである限り、再度辞書ではひかない。
  ;; (setq sdic-inline-not-search-style 't)     ; sdic-inline-delay に定められた秒数毎にポイント下の単語を辞書でひく。

  (defun ~sdic-inline-smart-display ()
    (interactive)
    (cond ((and transient-mark-mode mark-active)
           (~sdic-smart-describe))
          (t
           (setq sdic-inline-not-search-style 't)
           (sdic-inline-function))))
  
  )


(bundle emacswiki:sdic-inline-pos-tip)
(use-package sdic-inline-pos-tip
  :commands (sdic-inline-pos-tip-show)
  :init
  (setq sdic-inline-display-func 'sdic-inline-pos-tip-show))


;; Macの場合、OS標準の辞書が使える
(when (~is-mac)
  (defun ~dict-search-initial-input ()
    (thing-at-point 'word))

  (defun ~dict-search (query)
    (interactive (list (read-string "Enter query: " (~dict-search-initial-input))))
    (browse-url (concat "dict://" query)))

  (bind-key* "M-t" '~dict-search)

  (use-package pophint
    :config
    (pophint-thing:advice-thing-at-point-function ~dict-search-initial-input))
  )

