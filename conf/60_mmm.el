(use-package mmm-auto
  :straight mmm-mode
  :defer t
  :after (:any visual-basic-mode php-mode)
  :init
  (setq mmm-global-mode 'maybe)
  (setq mmm-submode-decoration-level 0)
  ;; (set-face-background 'mmm-default-submode-face "white smoke")

  (defun ~mmm-replace-auto-mode (from to)
    (setq auto-mode-alist
          (loop for e in auto-mode-alist
                if (eq (cdr e) from)
                collect (cons (car e) to)
                else
                collect e)))

  (defun ~mmm-regist-class-with-replace-auto-mode (mode new-major-mode class)
    (dolist (e auto-mode-alist)
      (when (eq (cdr e) mode)
        (mmm-add-mode-ext-class new-major-mode (car e) class)))
    (~mmm-replace-auto-mode mode new-major-mode))

  :config
  ;; For ASP
  (mmm-add-classes 
   `((asp-code
      :submode visual-basic-mode
      :front "<%[!=]?"
      :back "%>"
      :insert ((?% asp-code nil @ "<%" @ " " _ " " @ "%>" @)
               (?! asp-declaration nil @ "<%!" @ " " _ " " @ "%>" @)
               (?= asp-expression nil @ "<%=" @ " " _ " " @ "%>" @)))))

  (dolist (re '("\\.asp\\'"))
    (mmm-add-mode-ext-class 'web-mode re 'asp-code)
    (push `(,re . web-mode) auto-mode-alist))

  ;; For PHP
  (mmm-add-classes
   '((php-code
      :submode php-mode
      :front "<\\?\\(php\\)?"
      :back "\\?>")))

  (dolist (re '("\\.php[s345t]?\\'" "\\.phtml\\'"))
    (mmm-add-mode-ext-class 'web-mode re 'php-code)
    (push `(,re . web-mode) auto-mode-alist)))
