;; -*- coding: utf-8; -*-
(bundle org)
(bundle org-redmine)
(bundle org-ac)
(bundle org-link-travis)
(bundle org-linkany)
(use-package org
  :defer t

  :config
  
  (setq org-src-fontify-natively t)
  (setq org-tag-alist '(("@WORK" . ?w) ("@HOME" . ?h) ("Laptop" . ?l)))
  (setq org-agenda-custom-commands '(("f" occur-tree "FIXME")))

  (setq org-link-abbrev-alist
        '(("google"   . "http://www.google.com/search?q=")))

  (add-to-list 'org-export-backends 'md)

  (bind-keys :map org-mode-map
              ("C-j"   . nil)
              ("C-k"   . nil)
              ("C-S-f" . org-kill-line)
              ("C-<"   . org-mark-ring-goto)
              ("C->"   . org-open-at-point)

              ("C-c C-h"   . org-metaleft)
              ("C-c C-j"   . org-metadown)
              ("C-c C-k"   . org-metaup)
              ("C-c C-l"   . org-metaright)
              ("C-c C-S-h" . org-shiftmetaleft)
              ("C-c C-S-j" . org-shiftmetadown)
              ("C-c C-S-k" . org-shiftmetaup)
              ("C-c C-S-l" . org-shiftmetaright)

              ("C-c a" . org-agenda)
              ("C-c l" . org-insert-link)
              ("C-c L" . org-store-link)
              ("C-c g" . org-goto)
              ("C-c k" . org-kill-note-or-show-branches))

  (use-package org-ac
    :config
    (org-ac/config-default))

  ;; (use-package org-linkany
  ;;   :config
  ;;   (setq org-linkany/preferred-backend 'anything)
  ;;   (setq org-linkany/browse-function '~browse-url-externally))

  (use-package org-link-github-wiki)

  (use-package org-link-travis
    :config
    (setq org-link-travis/user-name "aki2o"))

  (use-package org-gcal)

  (use-package smartrep
    :defer t
    :config
    (smartrep-define-key 
        org-mode-map "C-c" '(("C-n" . (lambda () 
                                        (outline-next-visible-heading 1)))
                             ("C-p" . (lambda ()
                                        (outline-previous-visible-heading 1))))))

  (use-package pophint-config
    :defer t
    :config
    (pophint-config:set-tag-jump-command org-open-at-point))

  )


(use-package ox
  :defer t

  :config
  
  (defadvice org-md-link (after ~fix-for-github activate)
    ;; この機能不要かも
    (when ~org-md-export-for-github
      (ignore-errors
        (when (and (member (org-element-property :type (ad-get-arg 0)) '("http" "https" "file"))
                   (string-match (rx-to-string `(and "." (or "jpg" "jpeg" "png" "gif") eos))
                                 (downcase (org-element-property :path (ad-get-arg 0))))
                   (string-match "\\`\\[" ad-return-value))
          (setq ad-return-value (concat "!" ad-return-value))))))

  ;; コードブロックの変換をGitHub向けに変更
  (defvar ~org-md-export-for-github t)
  (defun ~org-toggle-md-export-for-github ()
    (interactive)
    (setq ~org-md-export-for-github (not ~org-md-export-for-github)))

  (defun ~org-md-src-block (src-block contents info)
    (if (not ~org-md-export-for-github)
        (org-md-example-block src-block contents info)
      (let* ((lang (org-element-property :language src-block))
             (lang (cond ((not (stringp lang))   "")
                         ((string= lang "cperl") "perl")
                         (t                      lang)))
             (value (org-element-property :value src-block)))
        (format "```%s\n%s```" lang value))))

  (dolist (b org-export-registered-backends)
    (when (eq (org-export-backend-name b) 'md)
      (let* ((tr-alist (org-export-backend-transcoders b))
             (src (assq 'src-block tr-alist))
             (tr-alist (delq src tr-alist)))
        (push '(src-block . ~org-md-src-block) tr-alist)
        (setf (org-export-backend-transcoders b) tr-alist))))
  
  )


(bundle org-gcal)
(use-package org-gcal
  :defer t
  :init
  (defvar ~org-gcal-directory (concat user-emacs-directory "org-gcal/"))
  (defvar ~org-gcal-main-schedule-file (concat ~org-gcal-directory "main.org"))
  (setq org-gcal-file-alist `(("ootsuhiroaki@gmail.com" . ,~org-gcal-main-schedule-file)))
  (setq cfw:org-capture-template `("c"
                                   "calfw2org"
                                   entry
                                   (file ,~org-gcal-main-schedule-file)
                                   "*  %?\n %(cfw:org-capture-day)")))


(use-package calfw-org
  :commands (cfw:org-create-file-source)
  :init
  (defun ~cfw:open-calendar ()
    (interactive)
    (when (not org-gcal-client-secret)
      (setq org-gcal-client-id (~auth-source-get-property 'id :app "org-gcal"))
      (setq org-gcal-client-secret (~auth-source-get-property 'secret :app "org-gcal")))
    (let ((src (cfw:org-create-file-source "予定" ~org-gcal-main-schedule-file nil)))
      (switch-to-buffer (cfw:cp-get-buffer (cfw:create-calendar-component-buffer
                                            :view 'month
                                            :contents-sources `(,src)))))))

