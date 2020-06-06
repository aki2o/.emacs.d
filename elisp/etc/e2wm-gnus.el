(eval-when-compile (require 'cl))
(require 'e2wm)
(require 'gnus)
(require 'gnus-direx nil t)
(require 'bbdb nil t)
(require 'bbdb- nil t)
(require 'pophint nil t)
(require 'pophint-config nil t)
(require 'inertial-scroll nil t)
(require 'smartrep nil t)
(require 'guide-key nil t)
(require 'log4e)
(require 'yaxception)

(defgroup e2wm-gnus nil
  "E2WM for Gnus."
  :group 'windows
  :prefix "e2wm-gnus:")

(defcustom e2wm-gnus:default-box-width 35
  "Number as the width of a box window."
  :type 'integer
  :group 'e2wm-gnus)

(defcustom e2wm-gnus:scroll-lines 10
  "Number as the amount of scroll vertically."
  :type 'integer
  :group 'e2wm-gnus)

(defcustom e2wm-gnus:scroll-columns 10
  "Number as the amount of scroll horizontally."
  :type 'integer
  :group 'e2wm-gnus)

(defcustom e2wm-gnus:preferred-box-backend 'gnus-direx
  "Feature of the preferred backend run as a box buffer."
  :type 'symbol
  :group 'e2wm-gnus)

(defcustom e2wm-gnus:preferred-addrbook-backend 'bbdb-
  "Feature of the preferred backend run as a address book."
  :type 'symbol
  :group 'e2wm-gnus)

(defcustom e2wm-gnus:popup-guide-p t
  "Whether pop-up guide using `guide-key.el'."
  :type 'boolean
  :group 'e2wm-gnus)

(defcustom e2wm-gnus:hok-move-only nil
  "Whether just move to hint without other action for the buffer."
  :type 'boolean
  :group 'e2wm-gnus)


(log4e:deflogger "e2wm-gnus" "%t [%l] %m" "%H:%M:%S" '((fatal . "fatal")
                                                       (error . "error")
                                                       (warn  . "warn")
                                                       (info  . "info")
                                                       (debug . "debug")
                                                       (trace . "trace")))
(e2wm-gnus--log-set-level 'trace)


(defvar e2wm-gnus:ignore-buffer-regexp-list nil)
(defvar e2wm-gnus::box-backend-buffer-name nil)
(defvar e2wm-gnus::addrbook-backend-buffer-name nil)
(defvar e2wm-gnus::active-box-buffer-name "")
(defvar e2wm-gnus::active-summary-buffer-name "")
(defvar e2wm-gnus::active-main-buffer-name "")
(defvar e2wm-gnus::regexp-summary-buffer "\\`\\*Summary ")
(defvar e2wm-gnus::regexp-article-buffer "\\`\\*Article\\(?:\\*\\'\\| \\)")
(defvar e2wm-gnus::regexp-server-buffer "\\`\\*Server\\*\\'")
(defvar e2wm-gnus::regexp-browse-buffer "\\`\\*Gnus Browse Server\\*\\'")

(defvar e2wm-gnus::current-window-name nil)
(make-variable-buffer-local 'e2wm-gnus::current-window-name)

(defvar e2wm-gnus::guide-key-original-position nil)


;;;;;;;;;;;;;
;; Utility

(defun* e2wm-gnus::show-message (msg &rest args)
  (apply 'message (concat "[E2WM-GNUS] " msg) args)
  nil)

(defmacro e2wm-gnus::awhen (test &rest body)
  (declare (indent 1))
  `(let ((it ,test)) (when it ,@body)))

(defmacro e2wm-gnus::awhen-buffer-live (buffnm &rest body)
  (declare (indent 1))
  `(let ((it (when ,buffnm (get-buffer ,buffnm))))
     (when (buffer-live-p it) ,@body)))

(defmacro e2wm-gnus::aif-buffer-live (buffnm then &rest else)
  (declare (indent 2))
  `(let ((it (when ,buffnm (get-buffer ,buffnm))))
     (if (buffer-live-p it) ,then ,@else)))

(defsubst e2wm-gnus::start-p ()
  (e2wm-gnus::awhen-buffer-live gnus-group-buffer
    (with-current-buffer it
      (eq major-mode 'gnus-group-mode))))

(defsubst e2wm-gnus::init-active-buffer (buffnm-sym)
  (when (and (symbol-value buffnm-sym)
             (not (buffer-live-p (get-buffer (symbol-value buffnm-sym)))))
    (e2wm-gnus--trace "init %s" (symbol-name buffnm-sym))
    (set buffnm-sym "")))

(defsubst e2wm-gnus::add-ignore-buffer-regexp (re)
  (add-to-list 'e2wm-gnus:ignore-buffer-regexp-list re t))

(defsubst e2wm-gnus::remove-ignore-buffer-regexp (re)
  (setq e2wm-gnus:ignore-buffer-regexp-list
        (delete re e2wm-gnus:ignore-buffer-regexp-list)))

(defsubst e2wm-gnus::is-ignore-buffer (buffnm)
  (loop for re in e2wm-gnus:ignore-buffer-regexp-list
        if (string-match re buffnm) return t))

(defsubst e2wm-gnus::is-message-buffer-name (buffnm)
  (and (stringp buffnm)
       (string-match "\\`\\*unsent " buffnm)))

(defsubst e2wm-gnus::is-message-buffer (buff)
  (when (buffer-live-p buff)
    (with-current-buffer buff
      (or (eq major-mode 'message-mode)
          (e2wm-gnus::is-message-buffer-name (buffer-name))))))

(defsubst e2wm-gnus::hide-window-if-shown (wname)
  (let ((wm (e2wm:pst-get-wm)))
    (when (wlf:get-window wm wname)
      (wlf:hide wm wname)
      t)))


;;;;;;;;;;;;;;;;;
;; Perspective

(defvar e2wm-gnus:recipe
  `(- (:upper-size-ratio 0.4)
      (| (:left-max-size ,e2wm-gnus:default-box-width)
         box
         (| (:left-size-ratio 0.5)
            (- (:upper-size-ratio 0.3)
               summary
               main)
            ref))
      sub))

(defvar e2wm-gnus:winfo
  '((:name box :plugin gnus-box)
    (:name summary :plugin gnus-summary :default-hide t)
    (:name main :plugin gnus-main :default-hide t)
    (:name ref :plugin gnus-reference :default-hide t)
    (:name sub :buffer nil :default-hide t)))

(defvar e2wm-gnus:dp-minor-mode-map
  (e2wm:define-keymap
   '(("prefix q" . e2wm-gnus:close-current-window)
     ("prefix f" . e2wm-gnus:hok)
     ("prefix h" . e2wm-gnus:scroll-left-window)
     ("prefix j" . e2wm-gnus:scroll-up-window)
     ("prefix k" . e2wm-gnus:scroll-down-window)
     ("prefix l" . e2wm-gnus:scroll-right-window)
     ) e2wm:prefix-key)
  "")

(e2wm:pst-class-register
 (make-e2wm:$pst-class :name       'gnus
                       :extend     'base
                       :title      "Gnus"
                       :init       'e2wm-gnus:dp-init
                       :start      'e2wm-gnus:dp-start
                       :leave      'e2wm-gnus:dp-leave
                       :update     'e2wm-gnus:dp-update
                       :switch     'e2wm-gnus:dp-switch
                       :popup      'e2wm-gnus:dp-popup
                       :keymap     'e2wm-gnus:dp-minor-mode-map))

(defun e2wm-gnus::set-main-buffer-if-alive (&optional noselect)
  (e2wm-gnus::hide-window-if-shown 'main)
  (e2wm-gnus::awhen-buffer-live e2wm-gnus::active-main-buffer-name
    (e2wm-gnus--trace "set to main buffer again")
    (e2wm:pst-buffer-set 'main it)))

(defun e2wm-gnus::set-summary-buffer-if-alive (&optional noselect)
  (e2wm-gnus::awhen-buffer-live e2wm-gnus::active-summary-buffer-name
    (e2wm-gnus--trace "set to summary buffer again")
    (e2wm:pst-buffer-set 'summary it)))

(defun e2wm-gnus::setup-article-buffer ()
  (local-set-key (kbd "q") 'e2wm-gnus:close-current-window))

(defun e2wm-gnus::setup-managed-buffer (buff &optional wname)
  (with-current-buffer buff
    (when wname
      (setq e2wm-gnus::current-window-name wname))
    (local-set-key (kbd "C-g") 'e2wm-gnus:keyboard-quit)
    (e2wm-gnus::config-guide-key-for-dp)))

(defun e2wm-gnus:dp-init ()
  (yaxception:$
    (yaxception:try
      (e2wm-gnus--trace "start dp init.")
      (let* ((gnus-wm (wlf:no-layout e2wm-gnus:recipe e2wm-gnus:winfo)))
        (when (e2wm:history-recordable-p e2wm:prev-selected-buffer)
          (e2wm:history-add e2wm:prev-selected-buffer))
        gnus-wm))
    (yaxception:catch 'error e
      (e2wm-gnus--error "failed dp init : %s\n%s"
                        (yaxception:get-text e)
                        (yaxception:get-stack-trace-string e))
      (yaxception:throw e))))

(defun e2wm-gnus:dp-start (wm)
  (yaxception:$
    (yaxception:try
      (e2wm-gnus--trace "start dp start.")
      (e2wm-gnus::init-active-buffer 'e2wm-gnus::active-box-buffer-name)
      (e2wm-gnus::init-active-buffer 'e2wm-gnus::active-summary-buffer-name)
      (e2wm-gnus::init-active-buffer 'e2wm-gnus::active-main-buffer-name)
      (e2wm-gnus::setup-guide-key)
      (add-hook 'gnus-article-mode-hook 'e2wm-gnus::setup-article-buffer t)
      (when (not (e2wm-gnus::start-p))
        (e2wm-gnus--trace "start gnus.")
        (gnus))
      (e2wm-gnus::set-summary-buffer-if-alive))
    (yaxception:catch 'error e
      (e2wm-gnus--error "failed dp start : %s\n%s"
                        (yaxception:get-text e)
                        (yaxception:get-stack-trace-string e))
      (yaxception:throw e))))

(defun e2wm-gnus:dp-leave (wm)
  (yaxception:$
    (yaxception:try
      (e2wm-gnus--trace "start dp leave.")
      (e2wm-gnus::reset-guide-key)
      (remove-hook 'gnus-article-mode-hook 'e2wm-gnus::setup-article-buffer))
    (yaxception:catch 'error e
      (e2wm-gnus--error "failed dp leave : %s\n%s"
                        (yaxception:get-text e)
                        (yaxception:get-stack-trace-string e))
      (yaxception:throw e))))

(defun e2wm-gnus:dp-update (wm)
  (yaxception:$
    (yaxception:try
      (e2wm-gnus--trace "start dp update.")
      (e2wm-gnus::awhen-buffer-live (e2wm:pst-buffer-get 'summary)
        (wlf:show wm 'summary))
      (e2wm-gnus::awhen-buffer-live (e2wm:pst-buffer-get 'main)
        (wlf:show wm 'main))
      (e2wm:$pst-class-super))
    (yaxception:catch 'error e
      (e2wm-gnus--error "failed dp update : %s\n%s"
                        (yaxception:get-text e)
                        (yaxception:get-stack-trace-string e))
      (yaxception:throw e))))

(defun e2wm-gnus:dp-switch (buff)
  (yaxception:$
    (yaxception:try
      (e2wm-gnus--trace "start dp switch. buff[%s]" buff)
      (e2wm-gnus::mng-buffer-display buff t))
    (yaxception:catch 'error e
      (e2wm-gnus--error "failed dp switch : %s\n%s"
                        (yaxception:get-text e)
                        (yaxception:get-stack-trace-string e))
      (yaxception:throw e))))

(defun e2wm-gnus:dp-popup (buff)
  (yaxception:$
    (yaxception:try
      (e2wm-gnus--trace "start dp popup. buff[%s]" buff)
      (e2wm-gnus::mng-buffer-display buff nil))
    (yaxception:catch 'error e
      (e2wm-gnus--error "failed dp popup : %s\n%s"
                        (yaxception:get-text e)
                        (yaxception:get-stack-trace-string e))
      (yaxception:throw e))))

(defun e2wm-gnus::mng-buffer-display (buff switchp)
  (e2wm-gnus--trace "start mng buffer display. buff[%s] switchp[%s]" buff switchp)
  (let ((buffnm (buffer-name buff))
        (mode (with-current-buffer buff major-mode)))
    (cond ((or (string= buffnm e2wm-gnus::box-backend-buffer-name)
               (memq mode '(gnus-server-mode gnus-browse-mode)))
           (e2wm-gnus--trace "set to box buffer")
           (setq e2wm-gnus::active-box-buffer-name buffnm)
           ;; continue gnus behavior
           nil)
          ((eq mode 'gnus-summary-mode)
           (e2wm-gnus--trace "set to summary buffer")
           (setq e2wm-gnus::active-summary-buffer-name buffnm)
           (e2wm:with-advice
            (e2wm:pst-buffer-set 'summary buff))
           ;; init active main buffer when summary is selected.
           (setq e2wm-gnus::active-main-buffer-name "")
           ;; continue gnus behavior
           nil)
          ((eq mode 'gnus-article-mode)
           (e2wm-gnus--trace "check whether set to main buffer")
           (when (not (e2wm-gnus::is-message-buffer-name e2wm-gnus::active-main-buffer-name))
             (e2wm-gnus--trace "set to main buffer")
             (setq e2wm-gnus::active-main-buffer-name buffnm)
             (e2wm:with-advice
              (e2wm:pst-buffer-set 'main buff)))
           ;; continue gnus behavior
           nil)
          ((e2wm-gnus::is-message-buffer buff)
           (e2wm-gnus--trace "set to main buffer")
           (setq e2wm-gnus::active-main-buffer-name buffnm)
           (e2wm:with-advice
            (e2wm:pst-buffer-set 'main buff))
           ;; continue gnus behavior
           nil)
          ((or (string= buffnm e2wm-gnus::addrbook-backend-buffer-name)
               (e2wm-gnus::awhen (buffer-file-name buff)
                 (file-exists-p it))
               (e2wm:document-buffer-p buff))
           (e2wm-gnus--trace "show as ref buffer")
           (e2wm-gnus::setup-managed-buffer buff 'ref)
           (e2wm:with-advice
            (e2wm:pst-buffer-set 'ref buff t switchp))
           t)
          ((e2wm-gnus::is-ignore-buffer buffnm)
           (e2wm-gnus--trace "buffer is ignored")
           t)
          (t
           (e2wm-gnus--trace "show as sub buffer")
           (e2wm-gnus::setup-managed-buffer buff 'sub)
           (e2wm:with-advice
            (e2wm:pst-buffer-set 'sub buff t switchp))
           t))))


;;;;;;;;;;;;
;; Plugin

(defun e2wm-gnus:def-plugin-box (frame wm winfo)
  (yaxception:$
    (yaxception:try
      (e2wm-gnus--trace "start def plugin box.")
      (e2wm-gnus::awhen-buffer-live e2wm-gnus::active-box-buffer-name
        (e2wm-gnus::setup-managed-buffer it)
        (with-current-buffer it
          (hl-line-mode t))
        (wlf:set-buffer wm (wlf:window-name winfo) it t)))
    (yaxception:catch 'error e
      (e2wm-gnus--error "failed def plugin box : %s\n%s"
                        (yaxception:get-text e)
                        (yaxception:get-stack-trace-string e))
      (yaxception:throw e))))

(e2wm:plugin-register 'gnus-box
                      "Gnus Group"
                      'e2wm-gnus:def-plugin-box)

(defun e2wm-gnus:def-plugin-summary (frame wm winfo)
  (yaxception:$
    (yaxception:try
      (e2wm-gnus--trace "start def plugin summary.")
      (e2wm-gnus::aif-buffer-live e2wm-gnus::active-summary-buffer-name
          (progn
            (e2wm-gnus::setup-managed-buffer it)
            (wlf:set-buffer wm (wlf:window-name winfo) it t))
        (wlf:hide wm (wlf:window-name winfo))))
    (yaxception:catch 'error e
      (e2wm-gnus--error "failed def plugin summary : %s\n%s"
                        (yaxception:get-text e)
                        (yaxception:get-stack-trace-string e))
      (yaxception:throw e))))

(e2wm:plugin-register 'gnus-summary
                      "Gnus Summary"
                      'e2wm-gnus:def-plugin-summary)

(defun e2wm-gnus:def-plugin-main (frame wm winfo)
  (yaxception:$
    (yaxception:try
      (e2wm-gnus--trace "start def plugin main.")
      (e2wm-gnus::aif-buffer-live e2wm-gnus::active-main-buffer-name
          (progn
            (e2wm-gnus::setup-managed-buffer it 'main)
            (wlf:set-buffer wm (wlf:window-name winfo) it t))
        (wlf:hide wm (wlf:window-name winfo))
        (setq e2wm-gnus::active-main-buffer-name "")))
    (yaxception:catch 'error e
      (e2wm-gnus--error "failed def plugin main : %s\n%s"
                        (yaxception:get-text e)
                        (yaxception:get-stack-trace-string e))
      (yaxception:throw e))))

(e2wm:plugin-register 'gnus-main
                      "Gnus Main"
                      'e2wm-gnus:def-plugin-main)

(defun e2wm-gnus:def-plugin-reference (frame wm winfo)
  (yaxception:$
    (yaxception:try
      (e2wm-gnus--trace "start def plugin ref.")
      (e2wm-gnus::aif-buffer-live e2wm-gnus::addrbook-backend-buffer-name
          (progn
            (e2wm-gnus::setup-managed-buffer it 'ref)
            (wlf:set-buffer wm (wlf:window-name winfo) it t))
        (wlf:hide wm (wlf:window-name winfo))))
    (yaxception:catch 'error e
      (e2wm-gnus--error "failed def plugin address book : %s\n%s"
                        (yaxception:get-text e)
                        (yaxception:get-stack-trace-string e))
      (yaxception:throw e))))

(e2wm:plugin-register 'gnus-reference
                      "Gnus Reference"
                      'e2wm-gnus:def-plugin-reference)


;;;;;;;;;;;;;;;;;;;;;;
;; Behavior of Gnus

(defmacro* e2wm-gnus::def-update-advice (func &key before-call after-call)
  (declare (indent 0))
  `(defadvice ,func (after e2wm-gnus:update activate)
    (when e2wm:pst-minor-mode
      (when (functionp ',before-call) (funcall ',before-call))
      (e2wm-gnus--trace "start e2wm:pst-update-windows after %s" (symbol-name ',func))
      (e2wm:pst-update-windows)
      (when (functionp ',after-call) (funcall ',after-call)))))

;; (defadvice gnus-summary-read-group (before e2wm-gnus:conf activate)
;;   (when e2wm:pst-minor-mode
;;     (setq e2wm-gnus::active-summary-buffer-name
;;           (gnus-summary-buffer-name (ad-get-arg 0)))
;;     (e2wm-gnus--trace "updated active summary buffer : %s" e2wm-gnus::active-summary-buffer-name)))

;; (defadvice gnus-article-setup-buffer (after e2wm-gnus:conf activate)
;;   (when e2wm:pst-minor-mode
;;     (setq e2wm-gnus::active-main-buffer-name gnus-article-buffer)
;;     (e2wm-gnus--trace "updated active main buffer : %s" e2wm-gnus::active-main-buffer-name)))


;;;;;;;;;;;;;;;;;;;
;; Other Feature

(defun e2wm-gnus::setup-box-backend ()
  (e2wm-gnus--trace "start setup box backend.")
  (let ((orgbufre (rx-to-string `(and bos ,gnus-group-buffer eos))))
    (cond ((and (eq e2wm-gnus:preferred-box-backend 'gnus-direx)
                (featurep 'gnus-direx))
           (setq e2wm-gnus::box-backend-buffer-name gnus-direx::buffer-name)
           (e2wm-gnus::add-ignore-buffer-regexp orgbufre))
          (t
           (setq e2wm-gnus::box-backend-buffer-name gnus-group-buffer)
           (e2wm-gnus::remove-ignore-buffer-regexp orgbufre)))))

(defun e2wm-gnus::setup-addrbook-backend ()
  (e2wm-gnus--trace "start setup addrbook backend.")
  (let ((orgbufre (rx-to-string `(and bos ,bbdb-buffer-name eos))))
    (cond ((and (eq e2wm-gnus:preferred-addrbook-backend 'bbdb-)
                (featurep 'bbdb-))
           (bbdb-:setup)
           (setq e2wm-gnus::addrbook-backend-buffer-name bbdb-::buffer-name)
           (e2wm-gnus::add-ignore-buffer-regexp orgbufre))
          (t
           (setq e2wm-gnus::addrbook-backend-buffer-name bbdb-buffer-name)
           (e2wm-gnus::remove-ignore-buffer-regexp orgbufre)))))

(defun e2wm-gnus::setup-smartrep ()
  (when (featurep 'smartrep)
    (smartrep-define-key
        e2wm-gnus:dp-minor-mode-map e2wm:prefix-key '(("h" . e2wm-gnus:scroll-left-window)
                                                      ("j" . e2wm-gnus:scroll-up-window)
                                                      ("k" . e2wm-gnus:scroll-down-window)
                                                      ("l" . e2wm-gnus:scroll-right-window)))))

(defun e2wm-gnus::config-guide-key (&rest keystrokes)
  (when (and e2wm-gnus:popup-guide-p
             (featurep 'guide-key))
    (dolist (keystr keystrokes)
      (guide-key/add-local-guide-key-sequence keystr))))

(defmacro e2wm-gnus::def-config-guide-key (target &rest keystrokes)
  `(defun ,(intern (concat "e2wm-gnus::config-guide-key-for-" (symbol-name target))) ()
     (e2wm-gnus::config-guide-key ,@keystrokes)))

(e2wm-gnus::def-config-guide-key dp e2wm:prefix-key)
(e2wm-gnus::def-config-guide-key summary "M" "B" "O" "S" "A" "H" "/")
(e2wm-gnus::def-config-guide-key bbdb "S")

(defun e2wm-gnus::setup-guide-key ()
  (when (featurep 'guide-key)
    (setq e2wm-gnus::guide-key-original-position guide-key/popup-window-position)
    (setq guide-key/popup-window-position 'bottom)
    (add-hook 'gnus-summary-mode-hook 'e2wm-gnus::config-guide-key-for-summary t)
    (add-hook 'bbdb-mode-hook 'e2wm-gnus::config-guide-key-for-bbdb t)))

(defun e2wm-gnus::reset-guide-key ()
  (when (featurep 'guide-key)
    (setq guide-key/popup-window-position e2wm-gnus::guide-key-original-position)
    (remove-hook 'gnus-summary-mode-hook 'e2wm-gnus::config-guide-key-for-summary)
    (remove-hook 'bbdb-mode-hook 'e2wm-gnus::config-guide-key-for-bbdb)))


;;;;;;;;;;;;;;;;;;
;; User Command

(defun e2wm-gnus:close-current-window ()
  (interactive)
  (when e2wm-gnus::current-window-name
    (e2wm-gnus::hide-window-if-shown e2wm-gnus::current-window-name)))

(defun e2wm-gnus:keyboard-quit ()
  (interactive)
  (yaxception:$
    (yaxception:try
      (keyboard-quit))
    (yaxception:finally
      (or (e2wm-gnus:close-current-window)
          (e2wm-gnus::hide-window-if-shown 'sub)
          (e2wm-gnus::hide-window-if-shown 'ref)
          (e2wm-gnus::hide-window-if-shown 'main)))))

(defun e2wm-gnus:hok ()
  (interactive)
  (if (or (not (functionp 'pophint:do))
          (not (boundp 'pophint:source-one-line)))
      (e2wm-gnus::show-message "Can't do hok. Maybe pophint.el is not setup yet.")
    (e2wm-gnus--trace "start pophint:do.")
    (pophint:do :source 'pophint:source-one-line
                :not-highlight t
                :allwindow t
                :action '(lambda (hint)
                           (funcall pophint--default-action hint)
                           (when (not e2wm-gnus:hok-move-only)
                             (cond
                              ((eq major-mode 'direx:direx-mode)
                               (direx:find-item))
                              ((eq major-mode 'gnus-group-mode)
                               (gnus-group-select-group))
                              ((eq major-mode 'gnus-server-mode)
                               (gnus-server-read-server))
                              ((eq major-mode 'gnus-browse-mode)
                               (gnus-browse-select-group))
                              ((eq major-mode 'gnus-summary-mode)
                               (gnus-summary-beginning-of-article))))))))

(defun e2wm-gnus::scroll-window (direction)
  (e2wm-gnus--trace "start scroll window. direction[%s]" direction)
  (let* ((wm (e2wm:pst-get-wm))
         (wnd (or (wlf:get-window wm 'sub)
                  (wlf:get-window wm 'ref)
                  (wlf:get-window wm 'main))))
    (when wnd
      (with-selected-window wnd
        (case direction
          (up    (if (commandp 'inertias-up)
                     (inertias-up)
                   (scroll-up e2wm-gnus:scroll-lines)))
          (down  (if (commandp 'inertias-down)
                     (inertias-down)
                   (scroll-down e2wm-gnus:scroll-lines)))
          (left  (scroll-left e2wm-gnus:scroll-columns))
          (right (scroll-right e2wm-gnus:scroll-columns)))))))

(defun e2wm-gnus:scroll-up-window ()
  (interactive)
  (e2wm-gnus::scroll-window 'up))

(defun e2wm-gnus:scroll-down-window ()
  (interactive)
  (e2wm-gnus::scroll-window 'down))

(defun e2wm-gnus:scroll-left-window ()
  (interactive)
  (e2wm-gnus::scroll-window 'left))

(defun e2wm-gnus:scroll-right-window ()
  (interactive)
  (e2wm-gnus::scroll-window 'right))


(defun e2wm-gnus:default-setup ()
  "Do the default setup of behavior for Gnus."
  (e2wm-gnus::def-update-advice gnus-group-select-group)
  (e2wm-gnus::def-update-advice gnus-summary-select-article
                                :before-call e2wm-gnus::set-main-buffer-if-alive)
  (e2wm-gnus::def-update-advice gnus-server-exit)
  (e2wm-gnus::def-update-advice gnus-browse-exit)
  (e2wm-gnus::def-update-advice gnus-summary-exit)
  (e2wm-gnus::def-update-advice gnus-summary-exit-no-update)
  (e2wm-gnus::def-update-advice gnus-summary-reply)
  (e2wm-gnus::def-update-advice gnus-summary-reply-with-original)
  (e2wm-gnus::def-update-advice message-kill-buffer)
  (e2wm-gnus::def-update-advice message-send-and-exit)

  (e2wm-gnus::setup-box-backend)
  (e2wm-gnus::setup-addrbook-backend)
  (e2wm-gnus::setup-smartrep)
  )


(provide 'e2wm-gnus)
