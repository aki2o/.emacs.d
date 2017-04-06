(bundle yaml-mode)
(use-package yaml-mode
  :defer t
  :init
  (mmask-regist-extension-with-icase 'yaml-mode "yml" "yaml")
  )


(bundle company-ansible)

