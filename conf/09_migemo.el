;; http://www.kaoriya.net/software/cmigemo
(use-package migemo
  :custom ((migemo-command "cmigemo")
           (migemo-options '("-q" "--emacs"))
           (migemo-dictionary (expand-file-name "/usr/local/share/migemo/utf-8/migemo-dict"))
           (migemo-user-dictionary nil)
           (migemo-regex-dictionary nil)
           (migemo-use-pattern-alist t)
           (migemo-use-frequent-pattern-alist t)
           (migemo-pattern-alist-length 1000)
           (migemo-coding-system 'utf-8-unix))
  :config
  (migemo-init))
