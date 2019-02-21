context("test-db")

test_that("db can connect to database using yaml config", {
  db_name <- "test.sqlite"
  prepare_example_db(db_name)
  on.exit(unlink(db_name))

  con <- db_connect("destination", ".")
  expect_true(DBI::dbIsValid(con))
})

test_that("dettl can connect to remote DB using yaml config and the vault", {
  srv <- vaultr::vault_test_server(if_disabled = stop)
  cl <- srv$client()
  cl$write("/secret/users/readonly", list(password = "readonly"))

  path <- tempfile()
  dir.create(path)
  filename <- file.path(path, "db_config.yml")
  cfg <- readLines("uat_config/db_config.yml")
  cfg_server <- gsub("<vault_server>", srv$addr, cfg, fixed = TRUE)
  writeLines(cfg_server, filename)

  withr::with_envvar(c(VAULTR_AUTH_METHOD = "token", VAULT_TOKEN = srv$token), {
    con <- db_connect("source", path)
    on.exit(DBI::dbDisconnect(con))
    expect_true(DBI::dbIsValid(con))
    expect_true(length(DBI::dbListTables(con)) > 0)
  })
})
