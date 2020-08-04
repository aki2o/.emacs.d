(use-package s :defer t)
(use-package f :defer t)
(use-package dash-functional :defer t)
(use-package ctable :defer t)
(use-package elmine :defer t)
(use-package hexrgb :defer t)
(use-package fuzzy :defer t)
(use-package dropdown-list :defer t)
(use-package mag-menu :defer t)
(use-package diminish :defer t)
(use-package gntp :defer t)


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


;;;;;;;;;;;;;
;; Command

;; move
(defun ~end-of-block ()
  (interactive)
  (backward-up-list)
  (forward-sexp))

(defun ~beginning-of-block ()
  (interactive)
  (backward-up-list))

(defvar ~find-definition-function 'xref-find-definitions-other-window)
(make-variable-buffer-local '~find-definition-function)

(with-eval-after-load 'pophint-tags
  (pophint-tags:advice-command xref-find-definitions-other-window))

(defun ~find-definition ()
  (interactive)
  (call-interactively ~find-definition-function))

(defvar ~find-references-function 'xref-find-references)
(make-variable-buffer-local '~find-references-function)

(defun ~find-references ()
  (interactive)
  (call-interactively ~find-references-function))

(defvar ~pop-marker-stack-function 'xref-pop-marker-stack)
(make-variable-buffer-local '~pop-marker-stack-function)

(defun ~pop-marker-stack ()
  (interactive)
  (call-interactively ~pop-marker-stack-function))

;; reference
(defvar ~popup-document-frame-function '~lsp-ui-doc-show)
(make-variable-buffer-local '~popup-document-frame-function)

(defun ~popup-document-frame ()
  (interactive)
  (call-interactively ~popup-document-frame-function))

(defvar ~popup-document-buffer-function '~lsp-ui-doc-dump-on-my-frame)
(make-variable-buffer-local '~popup-document-buffer-function)

(defun ~popup-document-buffer ()
  (interactive)
  (call-interactively ~popup-document-buffer-function))

(defvar ~focus-document-frame-function 'lsp-ui-doc-focus-frame)
(make-variable-buffer-local '~focus-document-frame-function)

(defun ~focus-document-frame ()
  (interactive)
  (call-interactively ~focus-document-frame-function))

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

(defvar ~tidy-code-current-function nil)
(make-variable-buffer-local '~tidy-code-current-function)

(defun ~tidy-code-current ()
  (interactive)
  (if ~tidy-code-current-function
      (call-interactively ~tidy-code-current-function)
    (error "Can't tidy code to current current on %s" (buffer-name))))

(defvar ~tidy-code-diff-files-function nil)
(make-variable-buffer-local '~tidy-code-diff-files-function)

(defun ~tidy-code-diff-files ()
  (interactive)
  (if ~tidy-code-diff-files-function
      (call-interactively ~tidy-code-diff-files-function)
    (error "Can't tidy code to diff files on %s" (buffer-name))))

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

(defun ~echo-git-diff-file-path-list ()
  (interactive)
  (message (mapconcat 'identity (~git-diff-path-list (current-buffer)) "\n")))

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

