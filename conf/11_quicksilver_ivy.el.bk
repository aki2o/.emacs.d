(use-package swiper)
(require 'ivy)

(setq ivy-height 15)
(setq ivy-fixed-height-minibuffer t)
(setq ivy-use-virtual-buffers t)

(setq ivy-minibuffer-map
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "C-m")   'ivy-done)
        (define-key map (kbd "C-z")   'ivy-call)
        (define-key map (kbd "C-g")   'minibuffer-keyboard-quit)
        (define-key map (kbd "SPC")   'self-insert-command)
        
        (define-key map (kbd "<down>") 'ivy-next-line)
        (define-key map (kbd "<up>")   'ivy-previous-line)
        (define-key map (kbd "C-j")    'ivy-next-line)
        (define-key map (kbd "C-k")    'ivy-previous-line)
        (define-key map (kbd "C-S-j")  'ivy-scroll-up-command)
        (define-key map (kbd "C-S-k")  'ivy-scroll-down-command)
        (define-key map (kbd "C-S-{")  'ivy-beginning-of-buffer)
        (define-key map (kbd "C-S-}")  'ivy-end-of-buffer)
        
        (define-key map (kbd "TAB")   'ivy-partial-or-done)
        (define-key map (kbd "C-S-l") 'ivy-partial-or-done)
        (define-key map (kbd "C-l")   'ivy-forward-char)
        (define-key map (kbd "C-d")   'ivy-backward-delete-char)
        (define-key map (kbd "C-S-h") 'ivy-backward-kill-word)
        
        (define-key map (kbd "C-f")   'ivy-delete-char)
        (define-key map (kbd "H-f")   'ivy-kill-word)
        (define-key map (kbd "C-S-f") 'ivy-kill-line)
        (define-key map (kbd "C-w")   'ivy-kill-ring-save)
        
        (define-key map (kbd "C-S-c q") 'ivy-alt-done)
        (define-key map (kbd "C-S-c Q") 'ivy-immediate-done)
        (define-key map (kbd "C-S-c n") 'ivy-next-line-or-history)
        (define-key map (kbd "C-S-c s") 'ivy-reverse-i-search)
        (define-key map (kbd "C-S-c h") 'ivy-next-history-element)
        (define-key map (kbd "C-S-c H") 'ivy-previous-history-element)
        (define-key map (kbd "C-S-c c") 'ivy-next-line-and-call)
        (define-key map (kbd "C-S-c C") 'ivy-previous-line-and-call)
        (define-key map (kbd "C-S-c m") 'ivy-toggle-regexp-quote)
        (define-key map (kbd "C-S-c y") 'ivy-yank-word)
        (define-key map (kbd "C-S-c I") 'ivy-insert-current)
        (define-key map (kbd "C-S-c b") 'hydra-ivy/body)
        (define-key map (kbd "C-S-c d") 'ivy-dispatching-done)
        (define-key map (kbd "C-S-c D") 'ivy-dispatching-call)
        (define-key map (kbd "C-S-c r") 'ivy-restrict-to-matches)
        (define-key map (kbd "C-S-c a") 'ivy-avy)
        (define-key map (kbd "C-S-c R") 'ivy-read-action)
        (define-key map (kbd "C-S-c o") 'ivy-occur)
        (define-key map (kbd "C-S-c i") 'ivy-toggle-ignore)
        (define-key map (kbd "C-S-c ?") 'ivy-help)
        map))

(custom-set-faces
 '(ivy-current-match ((t (:background "ForestGreen" :distant-foreground "black")))))

(delete (assoc 'counsel-M-x ivy-initial-inputs-alist) ivy-initial-inputs-alist)

;; (ivy-mode 1)
;; switch-to-buffer を置き換えたくないので、ivy-modeの他の処理だけ手動で実行
(setq completing-read-function 'ivy-completing-read)
(when ivy-do-completion-in-region
  (setq completion-in-region-function 'ivy-completion-in-region))

;; counsel が見つからないって言われたので、一旦コメントアウト
;; (require 'counsel)

;; (setq counsel-git-grep-map
;;       (let ((map (make-sparse-keymap)))
;;         (define-key map (kbd "C-p")     'counsel-git-grep-recenter)
;;         (define-key map (kbd "C-S-c r") 'counsel-git-grep-query-replace)
;;         (define-key map (kbd "C-S-c W") 'counsel-git-grep-switch-cmd)
;;         map))

;; (setq counsel-ag-map
;;       (let ((map (make-sparse-keymap)))
;;         (define-key map (kbd "C-p")     'counsel-git-grep-recenter)
;;         (define-key map (kbd "C-S-c r") 'counsel-git-grep-query-replace)
;;         map))

;; (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)

;; p-r

;; counsel が見つからないって言われたので、一旦コメントアウト
;;;; no set ivy-height

;; (defun counsel-yank-pop ()
;;   "Ivy replacement for `yank-pop'."
;;   (interactive)
;;   (if (eq last-command 'yank)
;;       (progn
;;         (setq ivy-completion-end (point))
;;         (setq ivy-completion-beg
;;               (save-excursion
;;                 (search-backward (car kill-ring))
;;                 (point))))
;;     (setq ivy-completion-beg (point))
;;     (setq ivy-completion-end (point)))
;;   (let ((candidates (cl-remove-if
;;                      (lambda (s)
;;                        (or (< (length s) 3)
;;                            (string-match "\\`[\n[:blank:]]+\\'" s)))
;;                      (delete-dups kill-ring))))
;;     (let ((ivy-format-function #'counsel--yank-pop-format-function))
;;       (ivy-read "kill-ring: " candidates
;;                 :action 'counsel-yank-pop-action
;;                 :caller 'counsel-yank-pop))))

;; (defun counsel-git-log ()
;;   "Call the \"git log --grep\" shell command."
;;   (interactive)
;;   (let ((counsel-async-split-string-re "\ncommit ")
;;         (counsel-yank-pop-truncate-radius 5)
;;         (ivy-format-function #'counsel--yank-pop-format-function))
;;     (ivy-read "Grep log: " #'counsel-git-log-function
;;               :dynamic-collection t
;;               :action #'counsel-git-log-action
;;               :unwind #'counsel-delete-process
;;               :caller 'counsel-git-log)))

;; マイナーモードが優先されてしまう

(defadvice ivy--minibuffer-setup (before ~override-minor-mode activate)
  (setq overriding-local-map ivy-minibuffer-map))

;; windowの復帰をaction側に任せてるっぽい

;; (defun ivy-call ()
;;   "Call the current action without exiting completion."
;;   (interactive)
;;   (unless ivy-inhibit-action
;;     (let ((action (ivy--get-action ivy-last)))
;;       (when action
;;         (let* ((collection (ivy-state-collection ivy-last))
;;                (x (cond ((and (consp collection)
;;                               (consp (car collection))
;;                               (cdr (assoc ivy--current collection))))
;;                         ((equal ivy--current "")
;;                          ivy-text)
;;                         (t
;;                          ivy--current))))
;;           (with-selected-window (or (active-minibuffer-window)
;;                                     (get-buffer-window))
;;             (save-selected-window
;;               (funcall action x))))))))

;; counsel が見つからないって言われたので、一旦コメントアウト
;;;; actionでは復帰とかしない

;; (defun counsel-git-grep-action (x)
;;   (when (string-match "\\`\\(.*?\\):\\([0-9]+\\):\\(.*\\)\\'" x)
;;     (let ((file-name (match-string-no-properties 1 x))
;;           (line-number (match-string-no-properties 2 x)))
;;       (find-file (expand-file-name file-name counsel--git-grep-dir))
;;       (goto-char (point-min))
;;       (forward-line (1- (string-to-number line-number)))
;;       (re-search-forward (ivy--regex ivy-text t) (line-end-position) t)
;;       (setf (ivy-state-window ivy-last) (get-buffer-window))
;;       (unless (eq ivy-exit 'done)
;;         (swiper--cleanup)
;;         (swiper--add-overlays (ivy--regex ivy-text))))))

;; windowを考慮できてない

(defun swiper--cleanup ()
  "Clean up the overlays."
  (while swiper--overlays
    (let ((ov (pop swiper--overlays)))
      (with-selected-window (overlay-get ov 'window)
        (save-excursion
          (goto-char (point-min))
          (isearch-clean-overlays)))
      (delete-overlay ov))))
