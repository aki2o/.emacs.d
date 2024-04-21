(require 'dash)

(defgroup bufloat nil
  "Display dialog made by frame."
  :group 'tools
  :group 'convenience)

(defcustom bufloat-vertical-position 'middle
  "Where to display the frame."
  :type '(choice (const :tag "Top" top)
                 (const :tag "Middle" middle)
                 (const :tag "Bottom" bottom))
  :group 'bufloat)

(defcustom bufloat-horizontal-position 'middle
  "Where to display the frame."
  :type '(choice (const :tag "Left" left)
                 (const :tag "Middle" middle)
                 (const :tag "Right" right))
  :group 'bufloat)

(defcustom bufloat-width 0.6
  "Number of columns or ratio of width of the frame."
  :type 'number
  :group 'bufloat)

(defcustom bufloat-height 0.6
  "Number of lines or ratio of height in the frame."
  :type 'number
  :group 'bufloat)

(defcustom bufloat-text-scale-level 0
  "Text scale amount for the buffer."
  :type 'integer
  :group 'bufloat)

(defcustom bufloat-border "white"
  "Border color of the frame."
  :type 'color
  :group 'bufloat)

(defface bufloat-header
  '((t :foreground "black"
       :background "deep sky blue"))
  "Face used on the header."
  :group 'bufloat)

(defface bufloat-background
  '((((background light)) :background "#b3b3b3")
    (t :background "#272A36"))
  "Background color of the documentation.
Only the `background' is used in this face."
  :group 'bufloat)

(defvar bufloat-frame-parameters
  '((left . -1)
    (top . -1)
    (width  . 0)
    (height  . 0)
    (min-width  . 1)
    (min-height  . 1)
    (left-fringe . 0)
    (right-fringe . 0)
    (menu-bar-lines . 0)
    (tool-bar-lines . 0)
    (tab-bar-lines . 0)
    (tab-bar-lines-keep-state . 0)
    (internal-border-width . 1)
    (vertical-scroll-bars . nil)
    (horizontal-scroll-bars . nil)
    (no-focus-on-map . t)
    (undecorated . t)
    (visibility . nil)
    (auto-raise . t)
    (mouse-wheel-frame . nil)
    (no-other-frame . t)
    (inhibit-double-buffering . t)
    (drag-internal-border . t)
    (no-special-glyphs . t)
    (desktop-dont-save . t)
    (user-size . t)
    (user-position . t)
    (name . ""))
  "Frame parameters used to create the frame.")

(defvar bufloat-after-open-hook nil
  "Hooks run on child-frame opened.
The functions receive 2 parameters: the frame and buffer.")


;;;;;;;;;;;;;
;; Utility

(defun bufloat--available-p ()
  (when (not (display-graphic-p))
    (error "Can't bufloat : not display-graphic-p"))
  (when (not (fboundp 'display-buffer-in-child-frame))
    (error "Can't bufloat : not display-buffer-in-child-frame")))

(defun bufloat--line-height (&optional line)
  (or (nth 2 (or (window-line-height line)
                 (and (redisplay t)
                      (window-line-height line))))
      0))


;;;;;;;;;;;;
;; Buffer

(cl-defun bufloat--render (buffer &key (header nil) (header-face 'bufloat-header))
  (with-current-buffer buffer
    (add-text-properties 1 (point-max) '(pointer arrow))
    (setq-local window-min-height 1)
    (setq-local show-trailing-whitespace nil)
    (setq-local window-configuration-change-hook nil)
    (when (boundp 'window-state-change-functions)
      (setq-local window-state-change-functions nil))
    (when (boundp 'window-state-change-hook)
      (setq-local window-state-change-hook nil))
    (setq-local window-size-change-functions nil)
    (setq-local face-remapping-alist `((header-line ,header-face)))
    (setq header-line-format header)
    (setq mode-line-format nil)
    (bufloat-frame-mode 1)
    (let ((text-scale-mode-step 1.1))
      (text-scale-set bufloat-text-scale-level))))


;;;;;;;;;;;
;; Frame

(defsubst bufloat--frame-p (&optional frame)
  (frame-parameter frame 'bufloat-orig-frame))

(defun bufloat--set-frame (frame)
  (set-frame-parameter nil 'bufloat-frame frame))

(cl-defun bufloat--get-frame (&key (or-raise nil) (check-alive t))
  (let ((frame (or (frame-parameter nil 'bufloat-frame)
                   (when (bufloat--frame-p) (selected-frame)))))
    (cond ((frame-live-p frame) frame)
          ((not check-alive) frame)
          (or-raise (error "Not found bufloat frame")))))

(defun bufloat--delete-frame ()
  (-when-let (frame (bufloat--get-frame :check-alive nil))
    (delete-frame frame)
    (bufloat--set-frame nil)))

(defsubst bufloat-original-frame ()
  (if (frame-parameter nil 'bufloat-frame)
      (selected-frame)
    (frame-parameter nil 'bufloat-orig-frame)))

(cl-defun bufloat--make-frame-for (buffer &key (minibuffer nil) (on-submit nil) (on-cancel nil))
  (let* ((after-make-frame-functions nil)
         (before-make-frame-hook nil)
         (params (append bufloat-frame-parameters
                         `((background-color . ,(face-background 'bufloat-background nil t))
                           (cursor-type . ,(frame-parameter nil 'cursor-type))
                           (bufloat-orig-frame . ,(selected-frame)) ; avoiding to use parent-frame because it looks to take focus when using minibuffer
                           (bufloat-orig-buffer . ,(current-buffer))
                           (bufloat-frame-buffer . ,buffer)
                           (bufloat-frame-on-submit . ,on-submit)
                           (bufloat-frame-on-cancel . ,on-cancel)
                           (minibuffer . ,minibuffer)
                           (unsplittable . ,(not minibuffer)))
                         ;; (when (not minibuffer)
                         ;;   `((default-minibuffer-frame . ,(selected-frame))
                         ))
         ;; (window (display-buffer-in-child-frame buffer `((child-frame-parameters . ,params))))
         ;; (frame (window-frame window))
         (frame (make-frame params))
         (window (frame-selected-window frame)))
    (display-buffer-record-window 'frame window buffer)
    (set-window-buffer window buffer)
    (set-window-dedicated-p window t)
    (when (not minibuffer)
      (redirect-frame-focus frame (selected-frame)))
    (set-face-background 'internal-border bufloat-border frame)
    (when (facep 'child-frame-border)
      (set-face-background 'child-frame-border bufloat-border frame))
    (set-face-background 'fringe nil frame)
    frame))

(cl-defun bufloat--move-frame (frame &key width height)
  (-let* (((left top _right _bottom) (window-edges nil t nil t))
          (frame-height (frame-pixel-height (bufloat-original-frame)))
          (frame-width (frame-pixel-width (bufloat-original-frame)))
          (char-h (frame-char-height frame))
          (char-w (frame-char-width frame))
          (max-height (floor (if (< height 1)
                                 (* frame-height height)
                               (* height char-h))))
          (max-width (floor (if (< width 1)
                                (* frame-width width)
                              (* width char-w))))
          (height (min frame-height max-height))
          (width (min frame-width max-width))
          ((left . top) (cons (pcase bufloat-horizontal-position
                                ('left (+ left char-w))
                                ('middle (max (- (/ frame-width 2) (/ width 2)) 10))
                                ('right (max (- frame-width width char-w) 10)))
                              (pcase bufloat-vertical-position
                                ('top (+ top char-h))
                                ('middle (max (- (/ frame-height 2) (/ height 2)) 10))
                                ('bottom (max (- frame-height height char-h) 10)))))
          (frame-resize-pixelwise t)
          (inhibit-redisplay t)
          (move-frame-functions nil)
          (window-size-change-functions nil)
          (window-state-change-hook nil)
          (window-state-change-functions nil)
          (window-configuration-change-hook nil))
    ;; Dirty way to fix unused variable in emacs 26
    (when window-state-change-functions
      window-state-change-hook)
    ;; Make frame invisible before moving/resizing it to avoid flickering:
    ;; We set the position and size in 1 call, modify-frame-parameters, but
    ;; internally emacs makes 2 different calls, which can be visible
    ;; to the user
    (and (frame-visible-p frame)
         (bufloat--size-and-pos-changed frame left top width height)
         (make-frame-invisible frame))
    (modify-frame-parameters
     frame
     `((width . (text-pixels . ,width))
       (height . (text-pixels . ,height))
       (left . (+ ,left))
       (top . (+ ,top))))
    (unless (frame-visible-p frame)
      (make-frame-visible frame))))

(defun bufloat--size-and-pos-changed (frame left top width height)
  (-let (((prev-left . prev-top) (frame-position frame)))
    (not (and (= left prev-left)
              (= top prev-top)
              (= height (frame-text-height frame))
              (= width (frame-text-width frame))))))


;;;;;;;;;;;;;;;;;;;
;; User Function

(defmacro bufloat-with-original-buffer (&rest body)
  (declare (indent 0))
  `(let* ((frame (bufloat--get-frame :or-raise t))
          (buffer (frame-parameter frame 'bufloat-orig-buffer)))
     (if (not (buffer-live-p buffer))
         (error "Not found bufloat original buffer")
       (with-current-buffer buffer
         ,@body))))

(defun bufloat-buffer ()
  (let* ((frame (bufloat--get-frame)))
    (when frame
      (frame-parameter frame 'bufloat-frame-buffer))))

(defun bufloat-focus-frame ()
  (select-frame-set-input-focus (bufloat--get-frame :or-raise t)))

(cl-defun bufloat-open (buffer &key
                               (on-submit nil)
                               (on-cancel nil)
                               (activate-minibuffer nil)
                               (header nil)
                               (header-face 'bufloat-header)
                               (width bufloat-width)
                               (height bufloat-height))
  (bufloat--available-p)
  (bufloat--delete-frame)
  (bufloat--render buffer :header header :header-face header-face)
  (let ((frame (bufloat--make-frame-for buffer :on-submit on-submit :on-cancel on-cancel :minibuffer activate-minibuffer)))
    (bufloat--set-frame frame)
    (bufloat--move-frame frame :width width :height height)
    (select-frame-set-input-focus frame)
    (run-hook-with-args 'bufloat-after-open-hook frame buffer)))

(defun bufloat-close (cancel)
  (interactive (list (when current-prefix-arg t)))
  (let* ((frame (bufloat--get-frame))
         (main-frame (bufloat-original-frame))
         (buffer (frame-parameter frame 'bufloat-frame-buffer))
         (on-submit (frame-parameter frame 'bufloat-frame-on-submit))
         (on-cancel (frame-parameter frame 'bufloat-frame-on-cancel))
         (callback (if cancel on-cancel on-submit)))
    (select-frame-set-input-focus main-frame)
    (bufloat--delete-frame)
    (when (functionp callback)
      (funcall callback buffer))
    (when (buffer-live-p buffer)
      (kill-buffer buffer))))

(defun bufloat-cancel ()
  (interactive)
  (bufloat-close t))


(defvar-keymap bufloat-frame-mode-map
  :doc "Keymap used on `bufloat-frame-mode'."
  "C-c C-c" #'bufloat-close
  "C-c C-k" #'bufloat-cancel)

(define-minor-mode bufloat-frame-mode
  "Minor mode for bufloat frame."
  :group 'bufloat :keymap bufloat-frame-mode-map)


(provide 'bufloat)
;;; bufloat.el ends here
