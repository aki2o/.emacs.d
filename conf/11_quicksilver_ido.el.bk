;; (bundle ido-completing-read+)
;; (bundle ido-ubiquitous)
;; (bundle ido-vertical-mode)
;; (bundle flx-ido)
;; (bundle smex)

;; (require 'ido)

;; (setq ido-enable-flex-matching t)
;; (setq ido-create-new-buffer 'always)
;; (setq ido-save-directory-list-file (concat user-emacs-directory "cache/ido.last"))
;; (setq ido-max-window-height 0.75)
;; (setq ido-auto-merge-work-directories-length nil) ; 別フォルダの同名ファイルを自動補完するクソ機能を止める
;; (setq confirm-nonexistent-file-or-buffer t)

;; (defun ido-init-completion-maps ()
;;   ;; Common map
;;   (let ((map (make-sparse-keymap)))
;;     (define-key map "\C-a"          'ido-toggle-ignore)
;;     (define-key map "\C-e"          'ido-edit-input)
;;     (define-key map "\t"            'ido-complete)
;;     (define-key map " "             'ido-complete-space)
;;     (define-key map (kbd "C-S-l")   'ido-exit-minibuffer)
;;     (define-key map "\C-j"          'ido-next-match)
;;     (define-key map "\C-k"          'ido-prev-match)
;;     (define-key map (kbd "C-S-c c") 'ido-toggle-case)
;;     (define-key map (kbd "C-S-c m") 'ido-toggle-regexp)
;;     (define-key map (kbd "C-S-c p") 'ido-toggle-prefix)
;;     (define-key map (kbd "C-S-c u") 'ido-undo-merge-work-directory)
;;     (define-key map (kbd "C-S-c r") 'ido-restrict-to-matches)
;;     (define-key map (kbd "C-S-c 1") 'ido-take-first-match)
;;     (define-key map [right]         'ido-next-match)
;;     (define-key map [left]          'ido-prev-match)
;;     (define-key map "?"             'ido-completion-help)
;;     (define-key map "\C-h"          'ido-magic-backward-char)
;;     (define-key map "\C-l"          'ido-magic-forward-char)
;;     (define-key map "\C-f"          'ido-magic-delete-char)
;;     (set-keymap-parent map minibuffer-local-map)
;;     (setq ido-common-completion-map map))
;;   ;; File and directory map
;;   (let ((map (make-sparse-keymap)))
;;     (define-key map [remap switch-to-buffer] 'ido-enter-switch-buffer)
;;     (define-key map [remap find-file]        'ido-fallback-command)
;;     (define-key map [remap dired]            'ido-enter-dired)
;;     (define-key map [down]                   'ido-next-match-dir)
;;     (define-key map [up]                     'ido-prev-match-dir)
;;     (define-key map [(meta up)]              'ido-prev-work-directory)
;;     (define-key map [(meta down)]            'ido-next-work-directory)
;;     (define-key map (kbd "C-S-h")            'ido-delete-backward-word-updir)
;;     (define-key map (kbd "C-S-c ^")          'ido-up-directory)
;;     (define-key map (kbd "C-S-c g")          'ido-reread-directory)
;;     (define-key map (kbd "C-S-c w")          'ido-wide-find-dir-or-delete-dir)
;;     (define-key map (kbd "C-S-c W")          'ido-wide-find-file-or-pop-dir)
;;     (define-key map (kbd "C-S-c a")          'ido-push-dir)
;;     (define-key map (kbd "C-S-c A")          'ido-push-dir-first)
;;     (define-key map (kbd "C-S-c f")          'ido-forget-work-directory)
;;     (define-key map (kbd "C-S-c n")          'ido-prev-work-file)
;;     (define-key map (kbd "C-S-c p")          'ido-next-work-file)
;;     (define-key map (kbd "C-S-c N")          'ido-next-work-directory)
;;     (define-key map (kbd "C-S-c P")          'ido-prev-work-directory)
;;     (define-key map (kbd "C-S-c m")          'ido-make-directory)
;;     (define-key map (kbd "C-S-c M")          'ido-merge-work-directories)
;;     (set-keymap-parent map ido-common-completion-map)
;;     (setq ido-file-dir-completion-map map))
;;   ;; File only map
;;   (let ((map (make-sparse-keymap)))
;;     (define-key map (kbd "C-S-c k")   'ido-delete-file-at-head)
;;     (define-key map (kbd "C-S-c c w") 'ido-copy-current-word)
;;     (define-key map (kbd "C-S-c c f") 'ido-copy-current-file-name)
;;     (define-key map (kbd "C-S-c l")   'ido-toggle-literal)
;;     (set-keymap-parent map ido-file-dir-completion-map)
;;     (setq ido-file-completion-map map))
;;   ;; Buffer map
;;   (let ((map (make-sparse-keymap)))
;;     (define-key map [remap find-file]        'ido-enter-find-file)
;;     (define-key map [remap switch-to-buffer] 'ido-fallback-command)
;;     (define-key map (kbd "C-S-c k")          'ido-kill-buffer-at-head)
;;     (define-key map (kbd "C-S-c v")          'ido-toggle-virtual-buffers)
;;     (set-keymap-parent map ido-common-completion-map)
;;     (setq ido-buffer-completion-map map)))

;; (ido-mode 1)
;; ;; (ido-mode 'buffers)
;; ;; (ido-everywhere 1)

;; ;; (defun ~ido-recentf ()
;; ;;   (interactive)
;; ;;   (if (find-file (ido-completing-read "Find recent file: " recentf-list))
;; ;;       (message "Opening file...")
;; ;;     (message "Aborting")))

;; ;; (bind-keys* ("C-x f" . ~ido-recentf))

;; (use-package ido-vertical-mode
;;   :init
;;   (ido-vertical-mode 1))

;; ;; ;; ファイル開くと、全部read-onlyになってしまうので、一旦コメントアウト
;; ;; ;; (use-package ido-ubiquitous
;; ;; ;;   :init
;; ;; ;;   (ido-ubiquitous-mode 1))

;; (use-package flx-ido
;;   :init
;;   (setq flx-ido-threshold 10000)
  
;;   (flx-ido-mode 1))

;; ;; (use-package smex
;; ;;   :bind (("M-x" . smex))
  
;; ;;   :init
;; ;;   (setq smex-save-file (concat user-emacs-directory "cache/.smex-items"))
  
;; ;;   (smex-initialize))
