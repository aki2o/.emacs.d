(use-package s :defer t)
(use-package f :defer t)
(use-package dash-functional :defer t)
(use-package ctable :defer t)
(use-package elmine :defer t)
(use-package hexrgb :defer t)
(use-package fuzzy :defer t)
(use-package dropdown-list :defer t)
(use-package diminish :defer t)
(use-package gntp :defer t)
(use-package deferred :defer t)
(use-package posframe :defer t)


;;;;;;;;;;;;;;
;; Function

;; 指定されたモードのauto-mode-alistに定義されているキーのリスト
(defun ~get-mode-masks (modesym)
  (loop for pair in auto-mode-alist
        for v = (car pair)
        for k = (cdr pair)
        if (eq k modesym)
        collect v))

(defun ~get-active-window-buffer ()
  (window-buffer (nth 0 (window-list nil 'neither))))

(defun ~get-active-window-file ()
  (let* ((buf (~get-active-window-buffer))
         (path (buffer-file-name buf)))
    (if (not path)
        (error "Not in file buffer.")
      (expand-file-name path))))

(defun ~case-invoke (func)
  (let* ((startpt (if (region-active-p)
                      (region-beginning)
                    (save-excursion (forward-word 1) (backward-word 1) (point))))
         (endpt (if (region-active-p)
                    (region-end)
                  (save-excursion (forward-word 1) (point))))
         (rep (funcall func (buffer-substring startpt endpt))))
    (save-excursion
      (kill-region startpt endpt)
      (goto-char startpt)
      (insert rep))))

(defun ~git-diff-path-list (buf)
  (-remove
   (lambda (e) (= (length e) 0))
   (split-string
    (with-current-buffer buf (shell-command-to-string "git diff HEAD --name-only"))
    "\n")))

(defun ~dwim-thing-at-point ()
  (cond ((use-region-p)
         (buffer-substring-no-properties (region-beginning) (region-end)))
        ((symbol-at-point)
         (substring-no-properties
          (symbol-name (symbol-at-point))))
        (t
         (word-at-point))))

(defun ~pulse-momentary ()
  (lexical-let* ((vbeg (save-excursion (beginning-of-visual-line) (point)))
                 (vend (save-excursion (end-of-visual-line) (point)))
                 (end (line-end-position))
                 (ov (make-overlay vbeg (if (= vend end) (1+ end) vend))))
    (overlay-put ov 'face 'highlight)
    (run-with-idle-timer 1 nil (lambda () (when ov (delete-overlay ov))))))

(cl-defmacro ~run-deferred (buffer &rest body)
  (declare (indent 1))
  `(progn
     (lexical-let ((buf ,buffer))
       (run-with-idle-timer 1 nil (lambda ()
                                    (when (buffer-live-p buf)
                                      (with-current-buffer buf ,@body)))))))

(cl-defmacro ~call-interactively-any-of (&rest commands)
  `(call-interactively (cl-loop for c in ',commands if (commandp c) return c)))

(cl-defmacro ~add-setup-hook (hook &rest body)
  (declare (indent 1))
  (let ((f (intern (format "~%s-setup" (eval hook))))
        (h (intern (format "%s-hook" (eval hook)))))
    `(progn
       (defun ,f () ,@body)
       (add-hook ',h ',f))))

(cl-defmacro ~add-setup-hook-for-load (feature hook &rest body)
  (declare (indent 2))
  (let ((f (intern (format "~%s-setup-for-%s" (eval hook) (eval feature))))
        (h (intern (format "%s-hook" (eval hook)))))
    `(progn
       (defun ,f () ,@body)
       (add-hook ',h ',f t))))

(cl-defmacro ~add-setup-hook-after-load (feature hook &rest body)
  (declare (indent 2))
  (let ((f (intern (format "~%s-setup-for-%s" (eval hook) (eval feature))))
        (h (intern (format "%s-hook" (eval hook)))))
    `(with-eval-after-load ,feature
       (defun ,f () ,@body)
       (add-hook ',h ',f t))))

(cl-defmacro my:defun-localized-command (name &optional default)
  (declare (indent 1))
  (let ((v (intern (format "~%s-function" name)))
        (f (intern (format "~%s" name))))
    `(progn
       (defvar ,v ,default)
       (make-variable-buffer-local ',v)
       (defun ,f ()
         (interactive)
         (if (commandp ,v)
             (call-interactively ,v)
           (error ,(format "No %s" 'v)))))))


;;;;;;;;;;;;;
;; Command

;; move
(defun ~beginning-of-line ()
  (interactive)
  (~call-interactively-any-of seq-beginning-of-line beginning-of-line))

(defun ~end-of-block ()
  (interactive)
  (backward-up-list)
  (forward-sexp))

(defun ~beginning-of-block ()
  (interactive)
  (backward-up-list))

(defun ~imenu ()
  (interactive)
  (~call-interactively-any-of consult-imenu helm-imenu imenu))

(defun ~xref-find-definitions ()
  (interactive)
  (xref-find-definitions-other-window (xref-backend-identifier-at-point (xref-find-backend))))

(defun ~xref-find-references ()
  (interactive)
  (xref-find-references (xref-backend-identifier-at-point (xref-find-backend))))

(my:defun-localized-command find-definition '~xref-find-definitions)
(my:defun-localized-command find-references '~xref-find-references)
(my:defun-localized-command pop-marker-stack 'xref-pop-marker-stack)

;; reference
(my:defun-localized-command popup-document-frame)
(my:defun-localized-command popup-document-buffer)
(my:defun-localized-command focus-document-frame)
(my:defun-localized-command dwim-at-point)
(my:defun-localized-command action-at-point)

;; scroll
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

(defun ~scroll-to-top-of-window ()
  (interactive)
  (recenter 0))

(defun ~scroll-to-down-of-window ()
  (interactive)
  (recenter -1))

;; split window
(defun ~split-window-horizontally-and-select ()
  (interactive)
  (split-window-horizontally) (other-window 1))

(defun ~split-window-vertically-and-select ()
  (interactive)
  (split-window-vertically) (other-window 1))

;; edit
(defun ~next-line-with-insert ()
  (interactive)
  (end-of-line)
  (newline-and-indent))

(defun ~backward-kill-line ()
  (interactive)
  (kill-region (point-at-bol) (point)))

(defvar my:lint-executable nil)
(make-variable-buffer-local 'my:lint-executable)

(cl-defun my:lint-default (&optional (file (~projectile-relative-path (current-buffer))))
  (interactive)
  (when (not my:lint-executable)
    (error "Can't lint code : not set my:lint-executable"))
  (let* ((default-directory (projectile-project-root))
         (cmd (if (functionp my:lint-executable)
                  (funcall my:lint-executable file)
                my:lint-executable)))
    (~dockerize-shell-command (format "%s %s" cmd (shell-quote-argument file)))))

(defun my:lint-diff-files-default ()
  (interactive)
  (cl-loop for file in (~git-diff-path-list (current-buffer))
           do (my:lint-default file)))

(my:defun-localized-command lint-current 'my:lint-default)
(my:defun-localized-command lint-diff-files 'my:lint-diff-files-default)

(defun ~set-mark-only ()
  (interactive)
  (push-mark nil t nil))

;; insert
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

;; kill
(defun ~kill-ring-save-file-path ()
  (interactive)
  (let ((path (~get-active-window-file)))
    (kill-new path)
    (message path)))

(defun ~kill-ring-save-file-name ()
  (interactive)
  (let ((path (~get-active-window-file)))
    (kill-new (file-name-nondirectory path))
    (message path)))

(defun ~kill-ring-save-file-path-in-project ()
  (interactive)
  (let* ((root-path (with-current-buffer (~get-active-window-buffer)
                      (projectile-project-root)))
         (re (rx-to-string `(and bos ,root-path)))
         (path (replace-regexp-in-string re "" (~get-active-window-file))))
    (kill-new path)
    (message path)))

;; echo
(defun ~echo-file-path ()
  (interactive)
  (message (~get-active-window-file)))

(defun ~echo-face-at-point ()
  (interactive)
  (message "%s" (get-char-property (point) 'face)))

(defun ~echo-properties-at-point ()
  (interactive)
  (message "%s" (text-properties-at (point))))

(defun ~echo-git-diff-file-path-list ()
  (interactive)
  (message (mapconcat 'identity (~git-diff-path-list (current-buffer)) "\n")))

(defun ~echo-current-sexp-macro-expand ()
  (interactive)
  (let ((lisp (macroexpand (elisp--preceding-sexp))))
    (with-current-buffer-window
        "*Macro Expansion*" nil nil
      (pp lisp)
      (emacs-lisp-mode)
      (read-only-mode))))

;; case
(defun ~case-upper ()
  (interactive)
  (call-interactively
   (if (region-active-p) 'upcase-region 'upcase-word)))

(defun ~case-lower ()
  (interactive)
  (call-interactively
   (if (region-active-p) 'downcase-region 'downcase-word)))

(defun ~case-capitalize ()
  (interactive)
  (call-interactively
   (if (region-active-p) 'capitalize-region 'capitalize-word)))

(defun ~case-capitalize-from-snake ()
  (interactive)
  (~case-invoke 's-upper-camel-case))

(defun ~case-snake ()
  (interactive)
  (~case-invoke 's-snake-case))

;; window
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

;; file
(defun ~revert-buffer-with-sudo ()
  (interactive)
  (let* ((file (buffer-file-name))
         (tramp-path (if (string-match ~tramp-path-regexp file)
                         (replace-regexp-in-string ~tramp-path-regexp "/sudo:" file)
                       (concat "/sudo::" file))))
    (find-alternate-file tramp-path)))
