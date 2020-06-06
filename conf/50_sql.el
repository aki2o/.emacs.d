(use-package sql-indent
  :after (sql)
  :config
  (setq sql-indent-offset 2)
  (sql-set-product "mysql") ; mysql, postgres, oracle, sqlite...
  )
