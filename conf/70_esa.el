(bundle aki2o/emacs-esa-cui :name esa-cui)
(bundle aki2o/esa-direx)
(use-package esa-direx
  :defer t
  
  :config

  (esa-cui:regist-team "mf"
                       :access-token (~auth-source-get-property 'token :app "esa.mf"))
  
  (esa-cui--log-enable-logging)
  (esa-direx--log-enable-logging))

