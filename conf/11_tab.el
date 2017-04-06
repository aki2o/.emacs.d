;; (require 'tabbar)
;; (tabbar-mode 1)

;; ;; タブ上でマウスホイール操作無効
;; (tabbar-mwheel-mode 0)

;; ;; グループ化しない
;; (setq tabbar-buffer-groups-function nil)

;; ;; 左に表示されるボタンを無効化
;; (dolist (btn '(tabbar-buffer-home-button
;;                tabbar-scroll-left-button
;;                tabbar-scroll-right-button))
;;   (set btn (cons (cons "" nil)
;;                  (cons "" nil))))

;; ;; タブの間隔
;; (setq tabbar-separator '(0.8))

;; ;; ウィンドウからはみ出たときスクロールさせるか
;; (setq tabbar-auto-scroll-flag t)

;; ;; キーバインド
;; (global-set-key (kbd "M-]") 'tabbar-forward-tab)
;; (global-set-key (kbd "M-[") 'tabbar-backward-tab)

;; ;; 同じモードのファイルだけ表示する
;; (defun ~tabbar-buffer-list ()
;;   (loop with currmode = major-mode
;;         for buff in (buffer-list)
;;         if (and (not (string-match "\\`\\s-+\\*" (buffer-name buff)))
;;                 (with-current-buffer buff
;;                   (eq currmode major-mode)))
;;         collect buff))
;; (setq tabbar-buffer-list-function '~tabbar-buffer-list)

;; ;; 外観変更
;; (set-face-attribute 'tabbar-default nil
;;                     :background (face-attribute 'mode-line-inactive :background))
;; (set-face-attribute 'tabbar-unselected nil
;;                     :background (face-attribute 'mode-line-inactive :background)
;;                     :foreground (face-attribute 'mode-line-inactive :foreground)
;;                     :box nil)
;; (set-face-attribute 'tabbar-selected nil
;;                     :background (face-attribute 'mode-line :background)
;;                     :foreground (face-attribute 'mode-line :foreground)
;;                     :weight 'semi-bold
;;                     ;; :box `(:line-width 1 :color ,(face-attribute 'default :background) :style pressed-button)
;;                     :box nil)
;; ;; (set-face-attribute 'tabbar-button nil
;; ;;                     :box '(:line-width 1 :color "gray72" :style released-button))


