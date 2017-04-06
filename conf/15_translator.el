;; (bundle emacswiki:text-translator)
;; (bundle emacswiki:text-translator-vars)
;; (bundle emacswiki:text-translator-load)
;; (use-package text-translator
;;   :bind* (("C-t"   . text-translator-translate-default)
;;           ("C-S-t" . text-translator-all-by-auto-selection))

;;   :config
  
;;   (setq text-translator-auto-selection-func 'text-translator-translate-by-auto-selection-enja)
;;   (add-to-list 'text-translator-site-data-alist 
;;                '("alc.co.jp_enja" "alcom.alc.co.jp" "/translate HTTP/1.1" "translate[src]=%s&translate[mode]=0" utf-8 "<textarea id=\"translate_dst\" rows=\"15\" name=\"translate\\[dst\\]\" cols=\"37\">\\([^<]*\\)</textarea>"))
  
;;   (use-package popup
;;     :config
;;     (setq text-translator-display-popup t))
  
;;   (use-package sequential-command-config
;;     :config
;;     (define-sequential-command seq-translate
;;       text-translator-translate-default text-translator-translate-last-string seq-return)
;;     (bind-key* "C-t" 'seq-translate))
  
;;   )


(bundle google-translate)
(use-package google-translate
  :bind* (("C-t" . ~google-translate-dwim))

  :commands (google-translate-translate)

  :init
  
  (defvar ~google-translate-english-regexp "\\`[[:ascii:]]+\\'")

  (defun ~google-translate-thing-at-point ()
    (thing-at-point 'word))
  
  (defun ~google-translate-dwim (&optional query)
    (interactive)
    (let* ((query (cond ((stringp query) query)
                        ((use-region-p)
                         (buffer-substring (region-beginning) (region-end)))
                        (t
                         (read-string "Google Translate: " (~google-translate-thing-at-point)))))
           (asciip (string-match ~google-translate-english-regexp query))
           (source-lang (if asciip "en" "ja"))
           (result-lang (if asciip "ja" "en")))
      (run-at-time 0.1 nil 'deactivate-mark)
      (google-translate-translate source-lang result-lang query)))

  :config

  (require 'google-translate-default-ui)

  (use-package pophint-config
    :config
    (pophint-config:set-thing-at-point-function ~google-translate-thing-at-point))
  
  )

