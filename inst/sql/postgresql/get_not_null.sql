SELECT
  c.table_name,
  c.column_name,
  CASE
    WHEN pg_get_serial_sequence(c.table_name, c.column_name) IS NULL THEN
      false
    ELSE
      true
  END as is_serial
FROM information_schema.columns c
JOIN information_schema.tables t
  ON c.table_schema = t.table_schema
  AND c.table_name = t.table_name
WHERE c.table_schema NOT IN ('pg_catalog', 'information_schema')
  AND t.table_type = 'BASE TABLE'
  AND c.table_schema = 'public'
  AND c.is_nullable = 'NO'
ORDER BY
  table_name
