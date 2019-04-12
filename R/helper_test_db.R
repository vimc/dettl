## Create a simple "people" table in the DB for testing the import process.
## Ideally this would be in a helper-*.R file in testthat directory but we
## need to be able to setup a DB for vignette.
prepare_example_db <- function(db_name, dir = getwd(), add_data = FALSE,
                               add_job_table = FALSE) {
  path <- file.path(dir, db_name)
  if (file.exists(path)) {
    ## Ensure we always start with a fresh DB
    file.remove(path)
  }
  con <- dbi_db_connect(RSQLite::SQLite(), path)
  people_query <- DBI::dbSendQuery(con,
                                   "CREATE TABLE people (
      id     INTEGER PRIMARY KEY,
      name   TEXT,
      age    INTEGER,
      height INTEGER
    )"
  )
  DBI::dbClearResult(people_query)
  if (add_data) {
    person <- data.frame(list(
      name = "Daisy",
      age = 34,
      height = 189
    ), stringsAsFactors = FALSE)

    DBI::dbWriteTable(con, "people", person, append = TRUE)
  }

  if (add_job_table) {
    job_query <- DBI::dbSendQuery(con,
                                  "CREATE TABLE jobs (
        id     INTEGER PRIMARY KEY,
        job    TEXT,
        person INTEGER,
        FOREIGN KEY (person) REFERENCES people(id)
      )"
    )
    DBI::dbClearResult(job_query)
  }

  DBI::dbDisconnect(con)
  path
}
