(when (~is-mac)
  (setq mac-option-modifier 'hyper)
  (setq mac-command-modifier 'meta)
  (define-key global-map [?¥] [?\\]))

;; デフォルトを空ける
(global-unset-key (kbd "M-s"))


;; キーバインド用のマイナーモードを作成
(defvar ~keyjack-mode-map (make-sparse-keymap))

(define-minor-mode ~keyjack-mode "My Global Key"
  :init-value nil
  :lighter " KJ"
  :keymap ~keyjack-mode-map)

(defvar ~keyjack-define-with-global-set-key t)

(defadvice global-set-key (after ~set-keyjack-mode-map activate)
  (define-key global-map (ad-get-arg 0) (ad-get-arg 1))
  (when ~keyjack-define-with-global-set-key
    (define-key ~keyjack-mode-map (ad-get-arg 0) (ad-get-arg 1))))

(add-hook 'find-file-hook '~keyjack-mode)
(add-hook 'shell-mode-hook '~keyjack-mode)
(global-set-key (kbd "<f11>") '~keyjack-mode)


;; キーバインド
(defvar ~custom-key-plists
  `(
    ;; 移動
    (:key "C-h"   :cmd backward-char       :jack t)
    (:key "C-j"   :cmd next-line           :jack t)
    (:key "C-k"   :cmd previous-line       :jack t)
    (:key "C-l"   :cmd forward-char        :jack t)
    (:key "C-S-h" :cmd backward-word       :jack t)
    (:key "C-S-j" :cmd scroll-up           :jack t)
    (:key "C-S-k" :cmd scroll-down         :jack t)
    (:key "C-S-l" :cmd forward-word        :jack t)
    (:key "C-S-a" :cmd ~scroll-right       :jack t)
    (:key "C-S-e" :cmd ~scroll-left        :jack t)
    (:key "C-M-n" :cmd forward-list        :jack t)
    (:key "C-M-S-n" :cmd backward-list        :jack t)
    (:key "C-{"   :cmd beginning-of-buffer :jack t)
    (:key "C-}"   :cmd end-of-buffer       :jack t)
    ;; 検索
    (:key "C-S-s" :cmd isearch-backward :jack t)
    (:key "M-s w" :cmd isearch-forward-word)
    (:key "M-s s" :cmd isearch-forward-symbol)
    (:key "M-s m" :cmd isearch-forward-regexp)
    (:key "M-s M" :cmd isearch-backward-regexp)
    (:key "M-s f" :cmd find-name-dired)
    ;; ウィンドウ
    (:key "C-z"   :cmd delete-window :jack t)
    (:key "C-S-z" :cmd delete-other-windows :jack t)
    (:key "C-S-o" :cmd other-window :jack t)
    (:key "C-p"   :cmd recenter :jack t)
    (:key "C-|"   :cmd ~split-window-horizontally-and-select)
    (:key "C--"   :cmd ~split-window-vertically-and-select)
    ;; 編集
    (:key "C-S-m" :cmd ~next-line-with-insert        :jack t :kind edit)
    (:key "C-d"   :cmd backward-delete-char-untabify :jack t :kind edit)
    (:key "C-f"   :cmd delete-char                   :jack t :kind edit)
    (:key "C-S-d" :cmd ~backward-kill-line           :jack t :kind edit)
    (:key "C-S-f" :cmd kill-line                     :jack t :kind edit)
    (:key "C-H-d" :cmd backward-kill-word            :jack t :kind edit)
    (:key "C-H-f" :cmd kill-word                     :jack t :kind edit)
    (:key "C-w"   :cmd kill-ring-save                :jack t :kind edit)
    (:key "C-S-w" :cmd kill-region                   :jack t :kind edit)
    (:key "C-S-y" :cmd yank-pop                      :jack t :kind edit)
    (:key "C-?"   :cmd redo                          :jack t :kind edit)
    (:key "C-r"   :cmd query-replace                 :jack t :kind edit)
    (:key "C-S-r" :cmd query-replace-regexp          :jack t :kind edit)
    (:key "C-S-g" :cmd keyboard-escape-quit          :jack t :kind edit) ; C-gの弱い版
    (:key "M-g"   :cmd abort-recursive-edit          :jack t :kind edit)
    (:key "C-v"   :cmd comment-dwim                  :jack t :kind edit)
    (:key "C-S-v" :cmd comment-box                   :jack t :kind edit)
    (:key "C-n"   :cmd align                         :jack t :kind edit)
    (:key "C-S-n" :cmd align-regexp                  :jack t :kind edit)
    (:key "C-S-i" :cmd toggle-input-method           :jack t :kind edit)
    (:key "C-x i" :cmd indent-region                 :jack t :kind edit)
    ;; 実行
    (:key "C-x x x" :cmd shell-command)
    (:key "C-x x a" :cmd async-shell-command)
    (:key "C-x x p" :cmd shell-command-on-region)
    (:key "C-x x l" :cmd ~shell-command-on-region-each-line)
    (:key "C-x x r" :cmd ~shell-command-on-region-with-replace)
    (:key "C-x x i" :cmd ~shell-command-insert-result)
    (:key "C-x x c" :cmd compile)
    (:key "C-x x I" :cmd executable-interpret)
    (:key "C-x x s" :cmd shell)
    ;; 行ジャンプ
    (:key "C-x j" :cmd goto-line :jack t)
    ;; マクロ
    (:key "C-(" :cmd kmacro-start-macro        :jack t :kind edit)
    (:key "C-)" :cmd kmacro-end-macro          :jack t :kind edit)
    (:key "C-0" :cmd kmacro-end-and-call-macro :jack t :kind edit)
    ;; タグジャンプ
    (:key "C-<"     :cmd pop-tag-mark)
    (:key "C->"     :cmd find-tag)
    (:key "C-x t t" :cmd visit-tags-table)
    (:key "C-x t l" :cmd list-tags)
    (:key "C-x t j" :cmd find-tag)
    (:key "C-x t b" :cmd pop-tag-mark)
    (:key "C-x t s" :cmd tags-search)
    (:key "C-x t r" :cmd tags-query-replace)
    ;; バッファ
    (:key "C-x C-S-f" :cmd revert-buffer    :jack t)
    (:key "C-x C-b"   :cmd switch-to-buffer :jack t)
    (:key "C-x b"     :cmd list-buffers     :jack t)
    ;; ファイル
    (:key "C-x f"   :cmd find-file-at-point :jack t)
    (:key "C-x C-d" :cmd dired :jack t)
    ;; (:key "C-x d"   :cmd ffap-list-directory)
    ;; (:key "C-x d"   :cmd dired-at-point)
    ;; (:key "M-f"     :cmd follow-delete-other-windows-and-split)
    ;; エラー移動
    (:key "C-b"   :cmd next-error :jack t)
    (:key "C-S-b" :cmd previous-error :jack t)
    ))

(dolist (e ~custom-key-plists)
  (let ((~keyjack-define-with-global-set-key (plist-get e :jack)))
    (global-set-key (kbd (plist-get e :key)) (plist-get e :cmd))))


;; 関数

(defun ~scroll-left ()
  (interactive)
  (scroll-left 10 t))

(defun ~scroll-right ()
  (interactive)
  (scroll-right 10 t))

(defun ~scroll-other-window ()
  (interactive)
  (scroll-other-window 10))

(defun ~scroll-other-window-down ()
  (interactive)
  (scroll-other-window-down 10))

(defun ~scroll-other-window-left ()
  (interactive)
  (other-window 1) (scroll-left 10 t) (other-window -1))

(defun ~scroll-other-window-right ()
  (interactive)
  (other-window 1) (scroll-right 10 t) (other-window -1))

(defun ~split-window-horizontally-and-select ()
  (interactive)
  (split-window-horizontally) (other-window 1))

(defun ~split-window-vertically-and-select ()
  (interactive)
  (split-window-vertically) (other-window 1))

(defun ~next-line-with-insert ()
  (interactive)
  (end-of-line)
  (newline-and-indent))

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

(defun ~backward-kill-line ()
  (interactive)
  (kill-region (point-at-bol) (point)))


