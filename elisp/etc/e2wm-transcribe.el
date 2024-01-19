;;; e2wm-transcribe.el --- Perspective of e2wm.el for work like transcribing

;; Copyright (C) 2016 Hiroaki Otsu

;; Author: Hiroaki Otsu <ootsuhiroaki@gmail.com>
;; Keywords: tools, window manager
;; URL: https://github.com/aki2o/e2wm-transcribe
;; Version: 0.0.1
;; Package-Requires: ((e2wm "1.3") (log4e "0.2.0") (yaxception "0.3.2"))

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
;; see <https://github.com/aki2o/e2wm-transcribe/blob/master/README.md>

;;; Dependency:
;; 
;; - e2wm.el ( see <https://github.com/kiwanami/emacs-window-manager> )
;; - yaxception.el ( see <https://github.com/aki2o/yaxception> )
;; - log4e.el ( see <https://github.com/aki2o/log4e> )

;;; Installation:
;;
;; Put this to your load-path.
;; And put the following lines in your .emacs or site-start.el file.
;; 
;; (require 'e2wm-transcribe)

;;; Configuration:
;; 
;; ;; Make config suit for you. About the config item, see Customization or eval the following sexp.
;; ;; (customize-group "e2wm-transcribe")
;; 

;;; Customization:
;; 
;; [EVAL] (autodoc-document-lisp-buffer :type 'user-variable :prefix "e2wm-transcribe:[^:]" :docstring t)
;;  *** END auto-documentation

;;; API:
;; 
;; [EVAL] (autodoc-document-lisp-buffer :type 'function :prefix "e2wm-transcribe:[^:]" :docstring t)
;;  *** END auto-documentation
;; [EVAL] (autodoc-document-lisp-buffer :type 'command :prefix "e2wm-transcribe:[^:]" :docstring t)
;;  *** END auto-documentation
;; [Note] Functions and variables other than listed above, Those specifications may be changed without notice.

;;; Tested On:
;; 
;; - Emacs ... GNU Emacs 24.3.1 (i686-pc-linux-gnu, GTK+ Version 3.4.2) of 2013-08-22 on chindi02, modified by Debian
;; - e2wm.el ... Version 1.3
;; - yaxception.el ... Version 0.3.2
;; - log4e.el ... Version 0.2.0


;; Enjoy!!!


(eval-when-compile (require 'cl))
(require 'e2wm)
(require 'yaxception)
(require 'log4e)


(defvar e2wm:c-transcribe-recipe e2wm:c-two-recipe)
(defvar e2wm:c-transcribe-winfo e2wm:c-two-winfo)

(defvar e2wm-transcribe:left-buffers nil)
(defvar e2wm-transcribe:right-buffers nil)
(defvar e2wm-transcribe:next-target-left-p nil)

(defvar e2wm-transcribe:inhibit-sub-display nil)
(defvar e2wm-transcribe:right-select-p nil)

(defsubst e2wm-transcribe:active-p ()
  (and (e2wm:managed-p)
       (eq (e2wm:$pst-name (e2wm:pst-get-instance)) 'transcribe)))

(defun e2wm-transcribe:handled-buffer-p (buf)
  (or (e2wm:document-buffer-p buf)
      (e2wm:history-recordable-p buf)))


(e2wm:pst-class-register
 (make-e2wm:$pst-class
  :name       'transcribe
  :extend     'two
  :title      "Transcribing"
  :main       'left
  :init       'e2wm:dp-transcribe-init
  :leave      'e2wm:dp-transcribe-leave
  :switch     'e2wm:dp-transcribe-switch
  :popup      'e2wm:dp-transcribe-popup
  :display    'e2wm:dp-transcribe-display
  :keymap     'e2wm:dp-transcribe-minor-mode-map))

(defun e2wm:dp-transcribe-init ()
  (let* ((wm (wlf:no-layout e2wm:c-transcribe-recipe e2wm:c-transcribe-winfo))
         (buf (or e2wm:prev-selected-buffer
                  (e2wm:history-get-main-buffer))))
    (wlf:set-buffer wm 'left buf)
    (wlf:set-buffer wm 'right (e2wm:history-get-prev buf))
    
    (ad-enable-advice 'wlf:set-buffer 'after 'e2wm-transcribe:record-buffer)
    (ad-activate 'wlf:set-buffer)
    
    (setq e2wm-transcribe:left-buffers nil)
    (setq e2wm-transcribe:right-buffers nil)

    wm))

(defun e2wm:dp-transcribe-leave (wm)
  (ad-disable-advice 'wlf:set-buffer 'after 'e2wm-transcribe:record-buffer)
  (ad-activate 'wlf:set-buffer))

(defun e2wm:dp-transcribe-switch (buf)
  (e2wm:message "#DP TRANSCRIBE switch : %s" buf)
  (e2wm:dp-transcribe-popup buf))

(defun e2wm:dp-transcribe-popup (buf)
  (e2wm:message "#DP TRANSCRIBE popup : %s" buf)
  (cond
   ((eql (wlf:get-buffer (e2wm:pst-get-wm) 'left) buf)
    (e2wm:with-advice
     (e2wm:pst-buffer-set 'right buf t))
    t)
   ((memq buf e2wm-transcribe:left-buffers)
    (e2wm:with-advice
     (e2wm:pst-buffer-set 'left buf t t))
    t)
   ((memq buf e2wm-transcribe:right-buffers)
    (e2wm:with-advice
     (e2wm:pst-buffer-set 'right buf t e2wm-transcribe:right-select-p))
    t)
   ((e2wm-transcribe:handled-buffer-p buf)
    (e2wm:with-advice
     (if e2wm-transcribe:next-target-left-p
         (progn
           (e2wm:pst-buffer-set 'left buf t t)
           (setq e2wm-transcribe:next-target-left-p nil))
       (e2wm:pst-buffer-set 'right buf t e2wm-transcribe:right-select-p)))
    t)
   ((not e2wm-transcribe:inhibit-sub-display)
    (e2wm:dp-two-popup-sub buf)
    t)))

(defun e2wm:dp-transcribe-display (buf)
  (e2wm:message "#DP TRANSCRIBE display : %s" buf)
  (e2wm:dp-transcribe-popup buf))

(defvar e2wm:dp-transcribe-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map e2wm:dp-two-minor-mode-map)
    (e2wm:add-keymap
     map
     '(("prefix L" . e2wm-transcribe:next-buffer-into-left-window-command)
       ("prefix R" . e2wm-transcribe:current-buffer-into-right-window-command))
     e2wm:prefix-key)
    map))

(defun e2wm-transcribe:next-buffer-into-left-window-command ()
  (interactive)
  (setq e2wm-transcribe:next-target-left-p t)
  (message "[E2WM] Next buffer will be shown on left window."))

(defun e2wm-transcribe:current-buffer-into-right-window-command ()
  (interactive)
  (let ((buf (current-buffer)))
    (cl-pushnew buf e2wm-transcribe:right-buffers)
    (switch-to-buffer buf)))

;;;###autoload
(defun e2wm-transcribe:dp ()
  (interactive)
  (e2wm:pst-change 'transcribe))


(defadvice wlf:set-buffer (after e2wm-transcribe:record-buffer disable)
  (when (e2wm-transcribe:active-p)
    (let ((wname (ad-get-arg 1))
          (buf (ad-get-arg 2)))
      (cl-case wname
        (left
         (cl-pushnew buf e2wm-transcribe:left-buffers)
         (when (member buf e2wm-transcribe:right-buffers)
           (setq e2wm-transcribe:right-buffers
                 (delete buf e2wm-transcribe:right-buffers))))
        (right
         (when (member buf e2wm-transcribe:left-buffers)
           (setq e2wm-transcribe:left-buffers
                 (delete buf e2wm-transcribe:left-buffers))))))))


(defun e2wm-transcribe--find-function-selecting-right (orig &rest args)
  (let ((e2wm-transcribe:right-select-p t))
    (save-selected-window
      (apply orig args))))

(advice-add 'find-function-do-it :around 'e2wm-transcribe--find-function-selecting-right)

(with-eval-after-load 'xref
  ;; `xref-pop-to-location' don't consider a case that not in current window after `switch-to-buffer'.
  ;; So making `xref--goto-char' run selecting the window.
  (defun e2wm-transcribe--xref--goto-char (orig &rest args)
    (with-selected-window (get-buffer-window (marker-buffer (nth 0 args)))
      (apply orig args)))

  (advice-add 'xref--goto-char :around 'e2wm-transcribe--xref--goto-char))


(with-eval-after-load 'consult
  ;; consult don't consider a case that not in the window after `consult--buffer-display'.
  (defun e2wm-transcribe--consult-buffer-display (buf &optional action)
    (switch-to-buffer buf action)
    (select-window (get-buffer-window buf)))

  (setq consult--buffer-display 'e2wm-transcribe--consult-buffer-display)

  ;; Once sub window is shown during consult, it will stay until consult is finished.
  ;; It makes user feel corrupting the window layout. So inhibiting sub window during consult.
  (defun e2wm-transcribe--consult-inhibiting-sub-display (orig &rest args)
    (let ((e2wm-transcribe:inhibit-sub-display t))
      (apply orig args)))

  (advice-add 'consult--read :around 'e2wm-transcribe--consult-inhibiting-sub-display))


(provide 'e2wm-transcribe)
;;; e2wm-transcribe.el ends here
