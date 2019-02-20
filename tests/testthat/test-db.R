context("test-db")

test_that("db can connect to database using yaml config", {
  db_name <- "test.sqlite"
  prepare_example_db(db_name)
  on.exit(unlink(db_name))

  con <- db_connect("destination", ".")
  expect_true(DBI::dbIsValid(con))
})

test_that("dettl can connect to remote DB using yaml config and the vault", {
  srv <- vaultr::vault_test_server()
  cl <- srv$client()
  cl$write("/secret/users/readonly", list(password = "readonly"))

  ## Ensure addr to vault is correct for this particular test run
  test_cfg <- "uat_config/db_config.yml"
  cfg <- readLines(test_cfg)
  expect_true("vault_server: <vault_server>" %in% cfg, sprintf(
    "Vault server template not found in test config %s, test misconfigured and
will fail. Add 'vault_server: <vault_server> to cfg to be overriden by server
URL for individual test. ", test_cfg
  ))
  cfg_server <- gsub(pattern = "<vault_server>", replace = srv$addr, x = cfg)
  writeLines(cfg_server, con = "uat_config/db_config.yml")
  on.exit(writeLines(cfg, con = "uat_config/db_config.yml"), add = TRUE)

  withr::with_envvar(c(VAULTR_AUTH_METHOD = "token", VAULT_TOKEN = srv$token), {
    con <- db_connect("source", "uat_config")
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    expect_true(DBI::dbIsValid(con))
    expect_true(length(DBI::dbListTables(con)) > 0)
  })
})
