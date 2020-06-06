(use-package yaml-mode
  :defer t
  :init
  (mmask-regist-extension-with-icase 'yaml-mode "yml" "yaml"))


(use-package company-ansible
  :defer t)
