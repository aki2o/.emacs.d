(use-package esal
  :straight (:host github :repo "aki2o/emacs-esal")
  :defer t)


(use-package esa-direx
  :straight (:host github :repo "aki2o/esa-direx")
  :defer t
  :config
  (esal-regist-team "mf"
                       :access-token (~auth-source-get-property 'token :app "esa.mf"))
  
  (esal--log-enable-logging)
  (esa-direx--log-enable-logging))
