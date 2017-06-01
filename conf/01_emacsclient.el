(use-package server
  
  :config

  (when (not (server-running-p))
    (server-start)

    ;; ;; 編集が終了したらアイコン化する
    ;; (defun iconify-emacs-when-server-is-done () (unless server-clients (iconify-frame)))
    ;; (add-hook 'server-done-hook 'iconify-emacs-when-server-is-done t)

    ;; Buffer `hogehoge' still has clients; kill it? (yes or no) とかいわれるのがうざいのをなおす
    ;; http://aki.issp.u-tokyo.ac.jp/itoh/hiChangeLog/html/2007-04.html#2007-04-09-1
    (remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)
    )

  )

