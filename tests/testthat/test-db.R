context("test-db")

test_that("db can connect to database using yaml config", {
  path <- prepare_test_import()

  con <- db_connect("test", path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_true(DBI::dbIsValid(con))
})

test_that("dettl DB args can be read from yaml config", {
  path <- setup_config(db_pw = "password")
  db_cfg <- dettl_db_args(path, "example")
  expect_identical(db_cfg$driver, RSQLite::SQLite)
  expect_match(db_cfg$args$dbname, ".+/test.sqlite")
  expect_equal(db_cfg$log_table, "data_import_log")

  db_cfg <- dettl_db_args(path, "uat")
  expect_identical(db_cfg$driver, RPostgres::Postgres)
  expect_identical(db_cfg$args$dbname, "montagu")
  expect_equal(db_cfg$log_table, "data_import_log")

  expect_error(
    dettl_db_args(path, "missing"),
    "Cannot find config for database missing."
  )

  err <- function(args, addr) stop("vault error")
  mockery::stub(dettl_db_args, "vaultr::vault_resolve_secrets", err)
  expect_error(dettl_db_args(path, "example"),
               "Failed to retrieve database info from vault:\n    vault error")
})

test_that("db type will default to first configured db if NULL", {
  path <- setup_config()
  db_cfg <- dettl_db_args(path)
  expect_identical(db_cfg$driver, RSQLite::SQLite)
  expect_match(db_cfg$args$dbname, ".+/test.sqlite")
  expect_equal(db_cfg$log_table, "data_import_log")
})

test_that("dettl DB args can be read from yaml config and the vault", {
  srv <- vaultr::vault_test_server()
  cl <- srv$client()
  cl$write("/secret/users/readonly", list(password = "test"))
  path <- setup_config(vault_server = srv$addr)

  withr::with_envvar(c(VAULTR_AUTH_METHOD = "token", VAULT_TOKEN = srv$token), {
    cfg <- dettl_db_args(path, "uat")
    expect_length(cfg, 3)
    expect_type(cfg$driver, "closure")
    expect_equal(cfg$driver, RPostgres::Postgres)
    expect_length(cfg$args, 5)
    expect_equal(cfg$args$dbname, "montagu")
    expect_equal(cfg$args$host, "https://example.com")
    expect_equal(cfg$args$port, 12345)
    expect_equal(cfg$args$user, "readonly")
    expect_equal(cfg$args$password, "test")
    expect_equal(cfg$log_table, "data_import_log")
  })
})

test_that("no transient db", {
  path <- temp_file()
  dir.create(path, FALSE, TRUE)
  config <- list(db = list(
    test = list(
      driver = "RSQLite::SQLite",
      args = list(dbname = ":memory:"),
      log_table = "log_table"
    )
  ))
  writeLines(yaml::as.yaml(config), file.path(path, "dettl_config.yml"))
  expect_error(
    dettl_db_args(path, "test"),
    "Cannot use a transient SQLite database with dettl"
  )
})

test_that("sql dilect connection can be identified", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  expect_equal(sql_dialect(con), "sqlite")

  con <- prepare_example_postgres_db(FALSE)
  on.exit(DBI::dbDisconnect(con))
  expect_equal(sql_dialect(con), "postgresql")
})
