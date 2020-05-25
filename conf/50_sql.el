(use-package sql-indent
  :after (sql)
  :defer t
  :config
  (setq sql-indent-offset 2)
  (sql-set-product "mysql") ; mysql, postgres, oracle, sqlite...
  )
