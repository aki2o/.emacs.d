;;; e2wm-perspb.el --- Plugin of e2wm.el for buffer list of perspective.el

;; Copyright (C) 2016  Hiroaki Otsu

;; Author: Hiroaki Otsu <ootsuhiroaki@gmail.com>
;; Keywords: tools, window manager
;; URL: https://github.com/aki2o/e2wm-perspb
;; Version: 0.0.1
;; Package-Requires: ((e2wm "1.2") (persp-mode "2.9.4") (dash "2.12.0") (concurrent "0.3.1") (yaxception "0.3.2"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; see <https://github.com/aki2o/e2wm-perspb/blob/master/README.md>

;;; Dependency:
;; 
;; - e2wm.el ( see <https://github.com/kiwanami/emacs-window-manager> )
;; - persp-mode.el ( see <https://github.com/Bad-ptr/persp-mode.el> )
;; - dash.el ( see <https://github.com/magnars/dash.el> )
;; - concurrent.el ( see <https://github.com/kiwanami/emacs-deferred> )
;; - yaxception.el ( see <https://github.com/aki2o/yaxception> )

;;; Installation:
;;
;; Put this to your load-path.
;; And put the following lines in your .emacs or site-start.el file.
;; 
;; (require 'e2wm-perspb)

;;; Configuration:
;; 
;; (setq e2wm:c-code-recipe
;;       '(| (:left-max-size 40)
;;           (- (:upper-size-ratio 0.6)
;;              files history)
;;           (- (:lower-max-size 150)
;;              (| (:right-max-size 40)
;;                 main imenu)
;;              sub)))
;; 
;; (setq e2wm:c-code-winfo
;;       '((:name main)
;;         (:name files   :plugin files)
;;         (:name history :plugin perspb)
;;         (:name imenu   :plugin imenu :default-hide nil)
;;         (:name sub     :buffer "*info*" :default-hide t)))

;;; Customization:
;; 
;; [EVAL] (autodoc-document-lisp-buffer :type 'user-variable :prefix "e2wm-perspb:[^:]" :docstring t)
;;  *** END auto-documentation

;;; API:
;; 
;; [EVAL] (autodoc-document-lisp-buffer :type 'function :prefix "e2wm-perspb:[^:]" :docstring t)
;;  *** END auto-documentation
;; [EVAL] (autodoc-document-lisp-buffer :type 'command :prefix "e2wm-perspb:[^:]" :docstring t)
;;  *** END auto-documentation
;; [Note] Functions and variables other than listed above, Those specifications may be changed without notice.

;;; Tested On:
;; 
;; - Emacs ... GNU Emacs 24.5.1 (x86_64-apple-darwin14.5.0, NS apple-appkit-1348.17) of 2016-06-16 on 192.168.102.190
;; - e2wm.el ... Version 1.2
;; - persp-mode.el ... Version 2.9.4
;; - dash.el ... Version 2.12.0
;; - concurrent.el ... Version 0.3.1
;; - yaxception.el ... Version 0.3.2


;; Enjoy!!!


(require 'cl)
(require 'e2wm)
(require 'persp-mode)
(require 'dash)
(require 'concurrent)
(require 'yaxception)


(defgroup e2wm-perspb nil
  "Plugin of e2wm.el for persp-mode."
  :group 'windows
  :prefix "e2wm-perspb:")

(defcustom e2wm-perspb:entry-makers '(e2wm-perspb-rails:make-entry
                                      e2wm-perspb:make-normal-entry)
  "List of function to make entry in e2wm-perspb:mode buffer."
  :type (list 'function)
  :group 'e2wm-perspb)

(defface e2wm-perspb:file-buffer-face
  '((t (:foreground "ivory")))
  "Face for file buffer."
  :group 'e2wm-perspb)

(defface e2wm-perspb:non-file-buffer-face
  '((t (:foreground "gray")))
  "Face for non-file buffer."
  :group 'e2wm-perspb)

(defface e2wm-perspb:current-highlight-face
  '((t (:background "gray30")))
  "Face for active entry."
  :group 'e2wm-perspb)


(defgroup e2wm-perspb-rails nil
  "Feature of e2wm-perspb for rails buffer."
  :group 'e2wm-perspb
  :prefix "e2wm-perspb-rails:")

(defcustom e2wm-perspb-rails:modes '(ruby-mode slim-mode haml-mode)
  "Major modes to activate e2wm-perspb-rails."
  :type (list 'symbol)
  :group 'e2wm-perspb-rails)

(defface e2wm-perspb-rails:controller-mark-face
  '((t (:background "lime green" :foreground "white" :bold t)))
  "Face for makr of controller."
  :group 'e2wm-perspb-rails)

(defface e2wm-perspb-rails:controller-name-face
  '((t (:foreground "lime green")))
  "Face for name of controller."
  :group 'e2wm-perspb-rails)

(defface e2wm-perspb-rails:model-mark-face
  '((t (:background "dodger blue" :foreground "white" :bold t)))
  "Face for mark of model."
  :group 'e2wm-perspb-rails)

(defface e2wm-perspb-rails:model-name-face
  '((t (:foreground "dodger blue")))
  "Face for name of model."
  :group 'e2wm-perspb-rails)

(defface e2wm-perspb-rails:view-mark-face
  '((t (:background "sandy brown" :foreground "white" :bold t)))
  "Face for mark of view."
  :group 'e2wm-perspb-rails)

(defface e2wm-perspb-rails:view-name-face
  '((t (:foreground "sandy brown")))
  "Face for name of view."
  :group 'e2wm-perspb-rails)

(defface e2wm-perspb-rails:other-app-mark-face
  '((t (:background "beige" :foreground "dark slate blue" :bold t)))
  "Face for mark of other under app."
  :group 'e2wm-perspb-rails)

(defface e2wm-perspb-rails:other-app-name-face
  '((t (:foreground "beige")))
  "Face for name of other under app."
  :group 'e2wm-perspb-rails)

(defface e2wm-perspb-rails:factory-mark-face
  '((t (:background "medium purple" :foreground "white" :bold t)))
  "Face for mark of factory."
  :group 'e2wm-perspb-rails)

(defface e2wm-perspb-rails:factory-name-face
  '((t (:foreground "medium purple")))
  "Face for name of factory."
  :group 'e2wm-perspb-rails)

(defface e2wm-perspb-rails:test-mark-face
  '((t (:background "violet red" :foreground "white" :bold t)))
  "Face for mark of test."
  :group 'e2wm-perspb-rails)

(defface e2wm-perspb-rails:test-name-face
  '((t (:foreground "violet red")))
  "Face for name of test."
  :group 'e2wm-perspb-rails)


(defun e2wm-perspb:make-normal-entry (buf)
  `(:name ,(buffer-name buf)
          :mark " "
          :name-face ,(if (buffer-file-name buf)
                          'e2wm-perspb:file-buffer-face
                        'e2wm-perspb:non-file-buffer-face)
          :mark-face nil))

(defun e2wm-perspb-rails:make-entry (buf)
  (when (memq (buffer-local-value 'major-mode buf) '(ruby-mode slim-mode haml-mode))
    (let ((filepath (expand-file-name (buffer-file-name buf)))
          (bufname (buffer-name buf)))
      (cond
       ((string-match "app/controllers/.+_controller\\.rb\\'" filepath)
        `(:name ,bufname
                :mark "C"
                :name-face e2wm-perspb-rails:controller-name-face
                :mark-face e2wm-perspb-rails:controller-mark-face))
       ((string-match "app/models/[^/]+\\.rb\\'" filepath)
        `(:name ,bufname
                :mark "M"
                :name-face e2wm-perspb-rails:model-name-face
                :mark-face e2wm-perspb-rails:model-mark-face))
       ((string-match "app/views/.+\\.html\\." filepath)
        `(:name ,bufname
                :mark "V"
                :name-face e2wm-perspb-rails:view-name-face
                :mark-face e2wm-perspb-rails:view-mark-face))
       ((string-match "app/.+\\.rb\\'" filepath)
        `(:name ,bufname
                :mark "A"
                :name-face e2wm-perspb-rails:other-app-name-face
                :mark-face e2wm-perspb-rails:other-app-mark-face))
       ((string-match "factories/.+\\.rb\\'" filepath)
        `(:name ,bufname
                :mark "F"
                :name-face e2wm-perspb-rails:factory-name-face
                :mark-face e2wm-perspb-rails:factory-mark-face))
       ((or (string-match "spec/.+_spec\\.rb\\'" filepath)
            (string-match "test/.+_test\\.rb\\'" filepath))
        `(:name ,bufname
                :mark "T"
                :name-face e2wm-perspb-rails:test-name-face
                :mark-face e2wm-perspb-rails:test-mark-face))))))
  

(defvar e2wm-perspb::buffer-name " *WM:Perspb*")

(e2wm:plugin-register 'perspb "Perspb" 'e2wm-perspb:def-plugin)

(defun e2wm-perspb:def-plugin (frame wm winfo)
  (let* ((wname (wlf:window-name winfo))
         (buf (e2wm-perspb::ensure-buffer))
         (wnd (get-buffer-window buf))
         (buf-list (-filter
                    'e2wm:history-recordable-p
                    (delete-if
                     'persp-buffer-filtered-out-p
                     (persp-buffer-list-restricted))))
         focused-buf)
    (when (window-live-p wnd)
      (with-selected-window wnd
        (yaxception:$~
          (yaxception:try
            (setq focused-buf (get-text-property (point-at-bol) 'e2wm:buffer))
            (setq buffer-read-only nil)
            (erase-buffer)
            (loop initially (goto-char (point-min))
                  with nextpt = nil
                  for b in buf-list
                  for pt = (point)
                  for entry = (loop for f in e2wm-perspb:entry-makers
                                    for entry = (funcall f b)
                                    if entry return entry
                                    finally return (e2wm-perspb:make-normal-entry b))
                  for line = (format "%s%s%s"
                                     (e2wm:rt (plist-get entry :mark) (plist-get entry :mark-face))
                                     (if (buffer-modified-p b) "*" " ")
                                     (e2wm:rt (plist-get entry :name) (plist-get entry :name-face)))
                  do (insert
                      (if (> pt (point-min)) "\n" "")
                      (e2wm:tp line 'e2wm:buffer b))
                  if (eql focused-buf b)
                  do (setq nextpt (point-at-bol))
                  finally do (goto-char (or nextpt (point-min))))
            (e2wm-perspb::update-current-highlight)
            (setq mode-line-format
                  '("-" mode-line-mule-info " " mode-line-position "-%-"))
            (setq header-line-format
                  (format "%s [%i]"
                          (or (e2wm:aif (get-current-persp)
                                  (persp-name it))
                              "none")
                          (length buf-list))))
          (yaxception:finally
            (setq buffer-read-only t)))))
    (wlf:set-buffer wm wname buf)))

(defun e2wm-perspb::ensure-buffer ()
  (let ((buf (get-buffer e2wm-perspb::buffer-name)))
    (if (and buf (buffer-live-p buf))
        buf
      (with-current-buffer (get-buffer-create e2wm-perspb::buffer-name)
        (e2wm-perspb:mode)
        (setq buffer-read-only t)
        (setq truncate-lines t)
        (buffer-disable-undo buf)
        (current-buffer)))))

(defvar e2wm-perspb::current-highlight nil)

(defun e2wm-perspb::update-current-highlight ()
  (when (not e2wm-perspb::current-highlight)
    (set (make-local-variable 'e2wm-perspb::current-highlight)
         (e2wm-perspb::make-current-highlight)))
  (move-overlay e2wm-perspb::current-highlight (point-at-bol) (1+ (point-at-eol))))

(defun e2wm-perspb::make-current-highlight ()
  (let ((ov (make-overlay (point) (point))))
    (overlay-put ov 'priority -50)
    (overlay-put ov 'face 'e2wm-perspb:current-highlight-face)
    ov))

(defvar e2wm-perspb:mode-map
  (e2wm:define-keymap
   '(
     ("p" . previous-line)
     ("n" . next-line)
     ("k" . previous-line)
     ("j" . next-line)
     
     ("C-m" . e2wm-perspb:select-command)
     ("q"   . e2wm:pst-window-select-main-command)
     )))

(define-derived-mode e2wm-perspb:mode fundamental-mode "Perspb")


;; User Command

(defun e2wm-perspb:select-command ()
  (interactive)
  (when (e2wm:managed-p)
    (let ((buf (get-text-property (point-at-bol) 'e2wm:buffer)))
      (switch-to-buffer buf)
      (e2wm:pst-window-select-main))))

(defun e2wm-perspb:switch-to-up-entry-command ()
  (interactive)
  (when (e2wm:managed-p)
    (let ((wm (e2wm:pst-get-wm))
          (wnd (get-buffer-window e2wm-perspb::buffer-name)))
      (when wnd
        (with-selected-window wnd
          (forward-line 1)
          (beginning-of-line)
          (e2wm-perspb:select-command))))))

(defun e2wm-perspb:switch-to-down-entry-command ()
  (interactive)
  (when (e2wm:managed-p)
    (let ((wm (e2wm:pst-get-wm))
          (wnd (get-buffer-window e2wm-perspb::buffer-name)))
      (when wnd
        (with-selected-window wnd
          (forward-line -1)
          (beginning-of-line)
          (e2wm-perspb:select-command))))))


(defvar e2wm-perspb::refresh-semaphore (cc:semaphore-create 1))

(defun e2wm-perspb:set-refresh-timer (frame-or-window)
  (when (> (cc:semaphore-permits e2wm-perspb::refresh-semaphore) 0)
    (cc:semaphore-acquire e2wm-perspb::refresh-semaphore)
    (run-with-idle-timer
     idle-update-delay
     nil
     '(lambda ()
        (yaxception:$~
          (yaxception:try
            (when (e2wm:managed-p)
              (e2wm:plugin-exec-update-by-plugin-name
               (selected-frame) (e2wm:pst-get-wm) 'perspb)))
          (yaxception:finally
            (cc:semaphore-release e2wm-perspb::refresh-semaphore)))))))

;; For perspective.el

(defadvice persp-add-buffer (after e2wm-perspb:refresh activate)
  (e2wm-perspb:set-refresh-timer 'frame))

(add-to-list 'persp-activated-functions 'e2wm-perspb:set-refresh-timer t)


(provide 'e2wm-perspb)
;;; e2wm-perspb.el ends here
