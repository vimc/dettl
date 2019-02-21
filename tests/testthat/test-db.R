context("test-db")

test_that("db can connect to database using yaml config", {
  db_name <- "test.sqlite"
  prepare_example_db(db_name)
  on.exit(unlink(db_name))

  con <- db_connect("destination", ".")
  expect_true(DBI::dbIsValid(con))
})

test_that("dettl DB args can be read from yaml config", {
  dest_dat <- dettl_db_args("destination", "default_config")
  expect_identical(dest_dat$driver, RSQLite::SQLite)
  expect_match(dest_dat$args$dbname, ".+/default_config/dettl.sqlite")

  source_dat <- dettl_db_args("source", "default_config")
  expect_identical(source_dat$driver, RSQLite::SQLite)
  expect_match(source_dat$args$dbname, ".+/default_config/test.sqlite")
})

test_that("dettl DB args can be read from yaml config and the vault", {
  srv <- vaultr::vault_test_server()
  cl <- srv$client()
  cl$write("/secret/users/readonly", list(password = "test"))
  path <- setup_config(srv$addr)

  withr::with_envvar(c(VAULTR_AUTH_METHOD = "token", VAULT_TOKEN = srv$token), {
    cfg <- dettl_db_args("source", path)
    expect_length(cfg, 2)
    expect_type(cfg$driver, "closure")
    expect_equal(cfg$driver, RPostgres::Postgres)
    expect_length(cfg$args, 5)
    expect_equal(cfg$args$dbname, "montagu")
    expect_equal(cfg$args$host, "example.com")
    expect_equal(cfg$args$port, 12345)
    expect_equal(cfg$args$user, "readonly")
    expect_equal(cfg$args$password, "test")
  })
})
