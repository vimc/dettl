WITH unnested_confkey AS (
  SELECT oid, unnest(confkey) as confkey
  FROM pg_constraint
),
unnested_conkey AS (
  SELECT oid, unnest(conkey) as conkey
  FROM pg_constraint
)
select
  c.conname                   AS constraint_name,
  c.contype                   AS constraint_type,
  CASE
    WHEN constraint_ns.nspname = 'public' THEN
      tbl.relname
    ELSE
      concat(constraint_ns.nspname, '.', tbl.relname)
  END                         AS constraint_table,
  col.attname                 AS constraint_column,
  CASE
    WHEN referenced_ns.nspname = 'public' THEN
      referenced_tbl.relname
    ELSE
      concat(referenced_ns.nspname, '.', referenced_tbl.relname)
  END                         AS referenced_table,
  referenced_field.attname    AS referenced_column,
  pg_get_constraintdef(c.oid) AS definition,
  CASE
    WHEN pg_get_serial_sequence(concat(referenced_ns.nspname, '.', referenced_tbl.relname), referenced_field.attname) IS NULL THEN
      false
    ELSE
      true
  END AS ref_is_serial
FROM pg_constraint c
LEFT JOIN unnested_conkey con ON c.oid = con.oid
LEFT JOIN pg_class tbl ON tbl.oid = c.conrelid
LEFT JOIN pg_namespace constraint_ns ON constraint_ns.oid = tbl.relnamespace
LEFT JOIN pg_attribute col ON (col.attrelid = tbl.oid AND col.attnum = con.conkey)
LEFT JOIN pg_class referenced_tbl ON c.confrelid = referenced_tbl.oid
LEFT JOIN pg_namespace referenced_ns ON referenced_ns.oid = referenced_tbl.relnamespace
LEFT JOIN unnested_confkey conf ON c.oid = conf.oid
LEFT JOIN pg_attribute referenced_field ON (referenced_field.attrelid = c.confrelid AND referenced_field.attnum = conf.confkey)
WHERE c.contype = 'f';
