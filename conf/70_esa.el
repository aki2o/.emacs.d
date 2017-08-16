(bundle aki2o/emacs-esal :name esal)
(bundle aki2o/esa-direx)
(use-package esa-direx
  :defer t
  
  :config

  (esal-regist-team "mf"
                       :access-token (~auth-source-get-property 'token :app "esa.mf"))
  
  (esal--log-enable-logging)
  (esa-direx--log-enable-logging))

