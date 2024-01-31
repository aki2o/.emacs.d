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

(defvar e2wm-transcribe:inhibit-sub-display nil)
(defvar e2wm-transcribe:right-select-p nil)
(defvar e2wm-transcribe:next-left-p nil)

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
    
    (setq e2wm-transcribe:left-buffers nil)
    (setq e2wm-transcribe:right-buffers nil)

    wm))

(defun e2wm:dp-transcribe-leave (wm)
  )

(defun e2wm:dp-transcribe-switch (buf)
  (e2wm:message "#DP TRANSCRIBE switch : %s" buf)
  (e2wm:dp-transcribe-popup buf))

(defun e2wm:dp-transcribe-popup (buf)
  (e2wm:message "#DP TRANSCRIBE popup : %s" buf)
  (cond
   ((e2wm-transcribe:handled-buffer-p buf)
    (e2wm:with-advice
     (if e2wm-transcribe:next-left-p
         (e2wm:pst-buffer-set 'left buf t t)
       (e2wm:pst-buffer-set 'right buf t e2wm-transcribe:right-select-p)))
    (setq e2wm-transcribe:next-left-p nil)
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
    map))

;;;###autoload
(defun e2wm-transcribe:dp ()
  (interactive)
  (e2wm:pst-change 'transcribe))


(defun e2wm-transcribe--wrap-selecting-right (orig &rest args)
  (let ((e2wm-transcribe:right-select-p t))
    (save-selected-window
      (apply orig args))))


;; `find-function-do-it' don't consider a case that not in current window after `switch-to-buffer'.
(advice-add 'find-function-do-it :around 'e2wm-transcribe--wrap-selecting-right)

(with-eval-after-load 'xref
  ;; `xref-pop-to-location' don't consider a case that not in current window after `switch-to-buffer'.
  (advice-add 'xref-pop-to-location :around 'e2wm-transcribe--wrap-selecting-right)
  (advice-add 'xref-go-back :around 'e2wm-transcribe--wrap-selecting-right)
  (advice-add 'xref-go-forward :around 'e2wm-transcribe--wrap-selecting-right))


(with-eval-after-load 'consult
  ;; consult don't consider a case that not in the window when `consult-after-jump-hook'.
  ;; So making sure to select the window.
  (defun e2wm-transcribe--consult-jump-selecting-right ()
    (let* ((e2wm-transcribe:right-select-p t)
           (buf (current-buffer))
           (pt (point))
           (w (get-buffer-window buf)))
      (when (not (window-live-p w))
        (pop-to-buffer buf)
        (setq w (get-buffer-window buf)))
      (select-window w)
      (set-window-point w pt)))

  (add-to-list 'consult-after-jump-hook 'e2wm-transcribe--consult-jump-selecting-right)

  (defun e2wm-transcribe--consult-jump-keeping-left (&rest args)
    (e2wm:pst-window-select-main))

  (advice-add 'consult--jump :after 'e2wm-transcribe--consult-jump-keeping-left)

  ;; Once sub window is shown during consult, it will stay until consult is finished.
  ;; It makes user feel corrupting the window layout. So inhibiting sub window during consult.
  (defun e2wm-transcribe--consult-inhibiting-sub-display (orig &rest args)
    (let ((e2wm-transcribe:inhibit-sub-display t))
      (apply orig args)))

  (advice-add 'consult--read :around 'e2wm-transcribe--consult-inhibiting-sub-display))


(provide 'e2wm-transcribe)
;;; e2wm-transcribe.el ends here
