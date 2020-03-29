(bundle dash-at-point)
(use-package dash-at-point
  :init
  (global-set-key (kbd "M-'")   'dash-at-point)
  (global-set-key (kbd "M-\"") 'dash-at-point-with-docset))
