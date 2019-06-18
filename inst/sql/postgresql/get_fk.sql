WITH unnested_confkey AS (
  SELECT c.conname, confkey.confkey
  FROM pg_constraint AS c, UNNEST(confkey) AS confkey
)
SELECT c.conname                                     AS constraint_name,
       c.contype                                     AS constraint_type,
       tbl.relname                                   AS constraint_table,
       col.attname                                   AS constraint_column,
       referenced_table.relname                      AS referenced_table,
       referenced_field.attname                      AS referenced_column,
       pg_get_constraintdef(c.oid)                   AS definition
FROM pg_constraint c
       JOIN LATERAL UNNEST(c.conkey) WITH ORDINALITY AS u(attnum, attposition) ON TRUE
       JOIN pg_class tbl ON tbl.oid = c.conrelid
       JOIN pg_namespace sch ON sch.oid = tbl.relnamespace
       JOIN pg_attribute col ON (col.attrelid = tbl.oid AND col.attnum = u.attnum)
       LEFT JOIN pg_class referenced_table ON c.confrelid = referenced_table.oid
       LEFT JOIN unnested_confkey conf ON c.conname = conf.conname
       LEFT JOIN pg_attribute referenced_field ON referenced_field.attnum = conf.confkey AND referenced_field.attrelid = c.confrelid
WHERE c.contype = 'f'
ORDER BY constraint_table, constraint_column, referenced_table, referenced_column;
