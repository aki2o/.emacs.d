;; ;;; 印刷の設定
;; ;; この設定で M-x print-buffer RET などでの印刷ができるようになります
;; ;;
;; ;;  notepad に与えるパラメータの形式の設定
;; (define-process-argument-editing "notepad"
;;   (lambda (x) (general-process-argument-editing-function x nil t)))

;; (defun w32-print-region (start end
;; 				  &optional lpr-prog delete-text buf display
;; 				  &rest rest)
;;   (interactive)
;;   (let ((tmpfile (convert-standard-filename (buffer-name)))
;; 	   (w32-start-process-show-window t)
;; 	   ;; もし、dos 窓が見えていやな人は上記の `t' を `nil' にします
;; 	   ;; ただし、`nil' にすると Meadow が固まる環境もあるかもしれません
;; 	   (coding-system-for-write w32-system-coding-system))
;;     (while (string-match "[/\\]" tmpfile)
;; 	 (setq tmpfile (replace-match "_" t nil tmpfile)))
;;     (setq tmpfile (expand-file-name (concat "_" tmpfile "_")
;; 				       temporary-file-directory))
;;     (write-region start end tmpfile nil 'nomsg)
;;     (call-process "notepad" nil nil nil "/p" tmpfile)
;;     (and (file-readable-p tmpfile) (file-writable-p tmpfile)
;; 	    (delete-file tmpfile))))

;; (setq print-region-function 'w32-print-region)
