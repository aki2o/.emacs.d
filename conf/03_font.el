(let* ((asciifont "Monaco")
       (jpfont "Monaco")
       (height 130)
       (fontspec (font-spec :family asciifont))
       (jp-fontspec (font-spec :family jpfont)))
  (set-face-attribute 'default nil :family asciifont :height height)
  (set-fontset-font nil 'japanese-jisx0213.2004-1 jp-fontspec)
  (set-fontset-font nil 'japanese-jisx0213-2 jp-fontspec)
  (set-fontset-font nil 'katakana-jisx0201 jp-fontspec)
  (set-fontset-font nil '(#x0080 . #x024F) fontspec) 
  (set-fontset-font nil '(#x0370 . #x03FF) fontspec))

;; 記号をデフォルトのフォントにしない。(for Emacs 25.2)
(setq use-default-font-for-symbols nil)


;; 初期インストール時に (all-the-icons-install-fonts) をやる必要があるっぽい
(use-package all-the-icons
  :custom ((inhibit-compacting-font-caches t)
           (all-the-icons-scale-factor 1.2)
           (all-the-icons-default-adjust -0.2))
  :config
  (let ((value (assoc-default 'help-mode all-the-icons-mode-icon-alist)))
    (setq all-the-icons-mode-icon-alist
          (delq (assoc 'fundamental-mode all-the-icons-mode-icon-alist) all-the-icons-mode-icon-alist))
    (add-to-list 'all-the-icons-mode-icon-alist `(fundamental-mode  ,@value) t)))
