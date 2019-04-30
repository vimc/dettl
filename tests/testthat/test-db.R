context("test-db")

test_that("db can connect to database using yaml config", {
  db_name <- "test.sqlite"
  prepare_example_db(db_name)
  on.exit(unlink(db_name), add = TRUE)

  con <- db_connect("test", ".")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_true(DBI::dbIsValid(con))
})

test_that("dettl DB args can be read from yaml config", {
  path <- setup_config(db_pw = "password")
  db_cfg <- dettl_db_args(path, "example")
  expect_identical(db_cfg$driver, RSQLite::SQLite)
  expect_match(db_cfg$args$dbname, ".+/test.sqlite")

  db_cfg <- dettl_db_args(path, "uat")
  expect_identical(db_cfg$driver, RPostgres::Postgres)
  expect_identical(db_cfg$args$dbname, "montagu")

  expect_error(
    dettl_db_args(path, "missing"),
    "Cannot find config for database missing."
  )
})

test_that("db type will default to first configured db if NULL", {
  path <- setup_config()
  db_cfg <- dettl_db_args(path)
  expect_identical(db_cfg$driver, RSQLite::SQLite)
  expect_match(db_cfg$args$dbname, ".+/test.sqlite")
})

test_that("dettl DB args can be read from yaml config and the vault", {
  srv <- vaultr::vault_test_server()
  cl <- srv$client()
  cl$write("/secret/users/readonly", list(password = "test"))
  path <- setup_config(vault_server = srv$addr)

  withr::with_envvar(c(VAULTR_AUTH_METHOD = "token", VAULT_TOKEN = srv$token), {
    cfg <- dettl_db_args(path, "uat")
    expect_length(cfg, 2)
    expect_type(cfg$driver, "closure")
    expect_equal(cfg$driver, RPostgres::Postgres)
    expect_length(cfg$args, 5)
    expect_equal(cfg$args$dbname, "montagu")
    expect_equal(cfg$args$host, "https://example.com")
    expect_equal(cfg$args$port, 12345)
    expect_equal(cfg$args$user, "readonly")
    expect_equal(cfg$args$password, "test")
  })
})

test_that("no transient db", {
  path <- tempfile()
  on.exit(unlink(path, recursive = TRUE))
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
