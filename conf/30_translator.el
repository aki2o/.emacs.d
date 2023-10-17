(bundle google-translate)
(use-package google-translate
  :defer t
  :commands (google-translate-translate ~google-translate-dwim)
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

  (with-eval-after-load 'pophint-autoloads
    (pophint-thing:advice-thing-at-point-function ~google-translate-thing-at-point)))


(~browse-document-defun-for fundamental "https://eow.alc.co.jp/search"
  :name alc
  :body (concat "?q=" (mapconcat 'identity words "+")))

(~browse-document-defun-for prog "https://eow.alc.co.jp/search"
  :name alc
  :body (concat "?q=" (mapconcat 'identity words "+")))
