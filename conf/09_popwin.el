(bundle popwin)
(use-package popwin
  :defer t
  :init
  ;; (defvar my-buffer-displayed-winconf nil)
  ;; (defun my-display-buffer-func (buf &optional ignore)
  ;;  (setq my-buffer-displayed-winconf (current-window-configuration))
  ;;  (popwin:display-buffer buf))
  ;; (defun my-restore-window-configuration ()
  ;;  "display-bufferを呼ぶ前のウィンドウ構成に戻す"
  ;;  (interactive)
  ;;  (set-window-configuration my-buffer-displayed-winconf))
  ;; (defun my-restore-window-configuration-before-anything ()
  ;;  "display-bufferを呼ぶ前のウィンドウ構成に戻す"
  ;;  (interactive)
  ;;  (anything-exit-minibuffer)
  ;;  (set-window-configuration my-buffer-displayed-winconf))
  ;; ;; view-modeでqを押すとウィンドウ構成を戻す
  ;; (define-key view-mode-map (kbd "q") 'my-restore-window-configuration)
  ;; ;; 複数ウィンドウ表示時にpopwinでanythingすると、おかしい対応
  ;; (define-key anything-map (kbd "C-g") 'my-restore-window-configuration)
  ;; (define-key anything-map (kbd "<RET>") 'my-restore-window-configuration-before-anything)

                                        ;(setq display-buffer-function 'my-display-buffer-func)
  (setq display-buffer-function 'popwin:display-buffer)

  (setq popwin:popup-window-height 0.5)
  (setq popwin:popup-window-width 0.5)

  (setq anything-samewindow nil)
  ;; (setq anything-display-function 'my-display-buffer-func)

  :config
  (push '("*anything*" :position bottom) popwin:special-display-config)
  (push '("*anything complete*" :position right :width 0.5) popwin:special-display-config)
  (push '("*anything buffers*" :position right :width 0.5) popwin:special-display-config)
  (push '("*anything for buffers*" :position right :width 0.5) popwin:special-display-config)
  (push '("*anything for files*" :position bottom :height 0.6) popwin:special-display-config)
  (push '("*anything kill-ring*" :position bottom :height 0.6) popwin:special-display-config)
  (push '("*anything register*" :position bottom :height 0.6) popwin:special-display-config)
  (push '("*anything moccur*" :position bottom :height 0.4) popwin:special-display-config)
  (push '("*anything apropos*" :position bottom) popwin:special-display-config)
  (push '("*anything etags*" :position bottom :height 0.7) popwin:special-display-config)
  (push '("*anything imenu*" :position right :width 0.5) popwin:special-display-config)

  (push '("*Helm*" :position bottom) popwin:special-display-config)
  (push '("*Helm Completions*" :position right :width 0.5) popwin:special-display-config)
  (push '("*helm buffers*" :position right :width 0.5) popwin:special-display-config)
  (push '("*Helm Find Files*" :position bottom :height 0.6) popwin:special-display-config)
  (push '("*helm for files*" :position bottom :height 0.6) popwin:special-display-config)
  (push '("*helm kill ring*" :position bottom :height 0.6) popwin:special-display-config)
  (push '("*helm mark ring*" :position bottom :height 0.6) popwin:special-display-config)
  (push '("*helm apropos*" :position bottom) popwin:special-display-config)
  (push '("*helm imenu*" :position right :width 0.5) popwin:special-display-config)

  (push '(help-mode :position right :width 0.4 :dedicated t) popwin:special-display-config)
  (push '(Info-mode :position right :width 0.5 :dedicated t) popwin:special-display-config)
  (push '("\\*Man " :regexp t :position right :width 0.4 :noselect t) popwin:special-display-config)
  (push '("\\*perldoc " :regexp t :position right :width 0.4 :noselect t) popwin:special-display-config)
  (push '("\\*plsense help\\*" :regexp t :position right :width 0.4 :noselect t) popwin:special-display-config)
  (push '("*git-gutter:diff*" :position right :width 0.4 :noselect t) popwin:special-display-config)

  (push '("*Compile-Log*" :position bottom :height 0.4 :noselect t) popwin:special-display-config)
  (push '("*Backtrace*" :position bottom :height 0.7 :dedicated t) popwin:special-display-config)
  (push '(" *auto-async-byte-compile*" :position bottom :height 14 :noselect t) popwin:special-display-config)
  (push '("*expectations result*" :position bottom :height 0.7 :noselect t) popwin:special-display-config)
  (push '("*ert*" :position right :width 0.7 :noselect t) popwin:special-display-config)
  (push '("*Caskxy Result*" :position right :width 0.7 :noselect t) popwin:special-display-config)
  (push '("\\*el-get-build:" :regexp t :position bottom :height 0.7 :noselect t) popwin:special-display-config)

  (push '("*Shell Command Output*" :position bottom :height 0.7) popwin:special-display-config)
  (push '("*One-Key*" :position bottom :height 0.8 :noselect t) popwin:special-display-config)
  (push '("*Directory*" :position bottom :height 0.5) popwin:special-display-config)
  (push '("\\*Tail:" :regexp t :position bottom :height 0.4 :noselect t) popwin:special-display-config)
  (push '("\\*translated\\*" :regexp t :position bottom :height 0.4 :noselect t) popwin:special-display-config)
  (push '("\*Google Translate\*" :position bottom :height 0.5 :stick t) popwin:special-display-config)
  (push '("*Ediff Control Panel*" :position bottom) popwin:special-display-config)
  (push '("*sdic*" :position bottom :height 0.4) popwin:special-display-config)
  (push '("*markdown-output*" :position right :width 0.4 :noselect t) popwin:special-display-config)
  (push '(" *linkany*" :position bottom) popwin:special-display-config)
  (push '(log4e-mode :position bottom :height 0.8 :noselect t) popwin:special-display-config)
  (push '(bbdb-:mode :position right :width 0.4) popwin:special-display-config)
  (push '(direx:direx-mode :position left :width 0.2 :dedicated t) popwin:special-display-config)
  (push '(esa-direx:mode :position left :width 0.2 :dedicated t) popwin:special-display-config)
  (push '("\\*PlSense DirEX\\[R\\]" :regexp t :position bottom :height 0.4 :dedicated t) popwin:special-display-config)
  (push '("\\*tramp/" :regexp t :position bottom :height 0.4 :noselect t) popwin:special-display-config)

  ;; (push '("*BTS: Project*" :position bottom :height 0.5) popwin:special-display-config)
  ;; (push '("*BTS: Query*" :position bottom :height 0.5) popwin:special-display-config)
  ;; (push '("*BTS: Ticket*" :position bottom :height 0.8) popwin:special-display-config)

  ;; (push '("*Ibuffer*" :position right :width 0.5) popwin:special-display-config)
  ;; (push '("\\*grep\\*" :regexp t :position right :width 0.7 :dedicated t) popwin:special-display-config)

  ;; For avoid of bug of Emacs
  (when (string-match "\\`24\\.3" emacs-version)
    (setq popwin:close-popup-window-timer-interval 0.5)))
