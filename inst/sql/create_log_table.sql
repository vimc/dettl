CREATE TABLE import_log (
  name       TEXT,
  date       TIMESTAMP WITH TIME ZONE,
  comment    TEXT,
  git_user   TEXT,
  git_email  TEXT,
  git_branch TEXT,
  git_hash   CHARACTER(40)
);
