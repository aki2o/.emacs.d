;; ;; yaoddmuse
;; (require 'yaoddmuse)

;; (setq yaoddmuse-directory (concat user-emacs-directory "yaoddmuse"))
;; (setq yaoddmuse-username "HiroakiOtsu")

;; ;; ホストが見つからないとエラーになってしまう対処
;; (loop for e in yaoddmuse-wikis
;;       for wikinm = (nth 0 e)
;;       if (or (string= wikinm "RatpoisonWiki")
;;              (string= wikinm "StumpwmWiki"))
;;       do (delete e yaoddmuse-wikis))

;; (yaoddmuse-update-pagename t)

