(bundle sql-indent)
(eval-after-load "sql"
  '(progn
     (require 'sql-indent)
     (setq sql-indent-offset 2)
     (sql-set-product "mysql") ; mysql, postgres, oracle, sqlite...
     ))
