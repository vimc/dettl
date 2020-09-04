CREATE TABLE dettl_import_log (
  name       TEXT PRIMARY KEY,
  start_time REAL,
  end_time   REAL,
  duration   REAL,
  comment    TEXT,
  git_user   TEXT,
  git_email  TEXT,
  git_branch TEXT,
  git_hash   CHARACTER(40)
);
