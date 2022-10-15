(require 'help-mode)

;; Helpバッファはview-modeにする
(add-to-list 'help-mode-hook
             '(lambda () (view-mode 1))
             t)


;; WoMan
(setq woman-manpath '(
                      "c:/App/cygwin/usr/man"
                      "c:/App/cygwin/local/man"
                      "c:/App/cygwin/usr/autotool/devel/man"
                      "c:/App/cygwin/usr/local/man"
                      "c:/App/cygwin/usr/local/man/ja"
                      "c:/App/cygwin/usr/ssl/man"
                      "c:/App/cygwin/usr/X11R6/man"
                      "c:/App/cygwin/usr/man/ja"
                      ))

;; 初回起動が遅いので cache 作成。
(setq woman-cache-filename (expand-file-name "~/woman_cache"))

;; 新しく frame は作らない。
(setq woman-use-own-frame nil)

;; 保存できない文字に色を付ける
(defun ~check-encode-able (beg end)
  (interactive "r")
  (save-excursion
    (let* (
           (mycodingsystem buffer-file-coding-system)
           mychar
           mycharname
           (mycount 0)
          ;;;encoding に対応する charset のリストを取得する。
          ;;;Meadow2 (Emacs21) でも動くかどうか未確認
          ;;;うまくいかなければ、自分で対応を定義すれば良い
           (mycharsetlist (coding-system-get mycodingsystem 'safe-charsets))
           )
      (goto-char beg) ;;;リージョンの先頭に移動
      (while (< (point) end) ;;;リージョン内を順に調べる
        (setq mychar (following-char))
        (setq mycharsetname (char-charset mychar))
        ;;合成文字に対する処理。 Meadow2 (Emacs21) では不要かも????
        (if (equal 'composition mycharsetname)
            (setq mycharsetname
                  (char-charset (string-to-char
                                 (decompose-string (char-to-string mychar))))))
        ;;encode できない文字だったら色をつける
        (if (or (equal mycharsetlist t) (member mycharsetname mycharsetlist))
            nil ;;;encode できる時は何もしない。 encode できない時↓
          (overlay-put (make-overlay (point) (1+ (point))) 'face 'region)
          (setq mycount (1+ mycount)))
        (forward-char) ;;;次の文字へ
        )
      ;;結果の表示
      (if (< 0 mycount)
          (message "%s で encode できない文字が%d 個ありました。"
                   mycodingsystem mycount))
      (if transient-mark-mode
          (setq deactivate-mark t)) ;;;region を色つけしている時、色を解除
      )))

;;保存できない文字を置換する
(defun ~check-encode-able (beg end)
  (interactive "r")
  (save-excursion
    (let* (
           (mycodingsystem buffer-file-coding-system)
           mychar
           mycharname
           (mycount 0)
          ;;;encoding に対応する charset のリストを取得する。
          ;;;Meadow2 (Emacs21) でも動くかどうか未確認
          ;;;うまくいかなければ、自分で対応を定義すれば良い
           (mycharsetlist (coding-system-get mycodingsystem 'safe-charsets))
           )
      (goto-char beg) ;;;リージョンの先頭に移動
      (while (< (point) end) ;;;リージョン内を順に調べる
        (setq mychar (following-char))
        (setq mycharsetname (char-charset mychar))
        ;;合成文字に対する処理。 Meadow2 (Emacs21) では不要かも????
        (if (equal 'composition mycharsetname)
            (setq mycharsetname
                  (char-charset (string-to-char
                                 (decompose-string (char-to-string mychar))))))
        ;;encode できない文字だったら色をつける
        (if (or (equal mycharsetlist t) (member mycharsetname mycharsetlist))
            nil ;;;encode できる時は何もしない。 encode できない時↓
          (if (y-or-n-p (format "Delete %s?" (buffer-substring-no-properties
                                              (point) (1+ (point)))))
              (delete-region (point) (1+ (point)))
            (delete-region (point) (1+ (point)))
            (insert (read-from-minibuffer "Replace String: "))
            (setq mycount (1+ mycount))))
        (forward-char) ;;;次の文字へ
        )
      ;;結果の表示
      (if (< 0 mycount)
          (message "%s で encode できない文字が%d 個ありました。"
                   mycodingsystem mycount))
      )))

