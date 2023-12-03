(use-package sql-indent
  :after (sql)
  :custom ((sql-indent-offset 2))
  :config
  (sql-set-product "mysql") ; mysql, postgres, oracle, sqlite...
  )
