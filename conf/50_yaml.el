(use-package yaml-mode
  :defer t
  :init
  (with-eval-after-load 'mmask
    (mmask-regist-extension-with-icase 'yaml-mode "yml" "yaml")))
