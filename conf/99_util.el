(bundle s)
(bundle f)
(bundle dash-functional)
(bundle ctable)
(bundle elmine)
(bundle hexrgb)
(bundle fuzzy)
(bundle dropdown-list)
(bundle mag-menu)
(bundle diminish)


;; Function

;; 指定されたモードのauto-mode-alistに定義されているキーのリスト
(defun ~get-mode-masks (modesym)
  (loop for pair in auto-mode-alist
        for v = (car pair)
        for k = (cdr pair)
        if (eq k modesym)
        collect v))

(defun ~get-active-window-file ()
  (let* ((buf (window-buffer (nth 0 (window-list nil 'neither))))
         (path (buffer-file-name buf)))
    (if (not path)
        (error "Not in file buffer.")
      (expand-file-name path))))


;; Command

(defun ~insert-date ()
  (interactive)
  (insert (format-time-string "%Y-%m-%dT%R:%S+09:00" (current-time))))

(defun ~insert-file-name ()
  (interactive)
  (insert (file-name-sans-extension (file-name-nondirectory (~get-active-window-file)))))

(defun ~insert-programmatic-ident-from-file-name ()
  (interactive)
  (insert (s-upper-camel-case
           (file-name-sans-extension (file-name-nondirectory (~get-active-window-file))))))

(defun ~kill-ring-save-file-path ()
  (interactive)
  (let ((path (~get-active-window-file)))
    (kill-new path)
    (message path)))

(defun ~print-file-path ()
  (interactive)
  (message (~get-active-window-file)))

(defun ~print-face-at-point ()
  (interactive)
  (message "%s" (get-char-property (point) 'face)))

(defun ~window-resizer ()
  (interactive)
  (let ((dx (if (= (nth 0 (window-edges)) 0) 1 -1))
        (dy (if (= (nth 1 (window-edges)) 0) 1 -1))
        c)
    (catch 'end-flag
      (while t
        (message "currnet size[%dx%d]. input [h,l,j,k] to resize : "
                 (window-width) (window-height))
        (setq c (read-char))
        (cond ((= c ?l) (loop for i from 1 to 8 do (enlarge-window-horizontally dx)))
              ((= c ?h) (loop for i from 1 to 8 do (shrink-window-horizontally dx)))
              ((= c ?j) (loop for i from 1 to 8 do (enlarge-window dy)))
              ((= c ?k) (loop for i from 1 to 8 do (shrink-window dy)))
              (t
               ;; otherwise
               (message "Quit")
               (throw 'end-flag t)))))))


(unbind-key "M-u")
(bind-keys* ("M-u i d" . ~insert-date)
            ("M-u i f" . ~insert-file-name)
            ("M-u i p" . ~insert-programmatic-ident-from-file-name)
            ("M-u k f" . ~kill-ring-save-file-path)
            ("M-u p f" . ~print-file-path)
            ("M-u p F" . ~print-face-at-point)
            ("M-u w"   . ~window-resizer)
            ("M-u t s" . text-scale-decrease)
            ("M-u t S" . text-scale-increase))

