(bundle dash-at-point)
(use-package dash-at-point
  :defer t
  
  :config
  
  (global-set-key (kbd "M->")   'dash-at-point)
  (global-set-key (kbd "C-M->") 'dash-at-point-docset))
