context("vault")

test_that("secrets can be retrieved from vault", {
  srv <- vaultr::vault_test_server()
  cl <- srv$client()
  cl$write("/secret/users/alice", list(password = "ALICE"))
  cl$write("/secret/users/bob", list(password = "BOB"))

  config <- list(path = tempfile(),
                 vault_server = srv$addr)

  x <- list(name = "alice",
            password = "VAULT:/secret/users/alice:password")
  withr::with_envvar(c(VAULTR_AUTH_METHOD = NA_character_), {
    expect_error(resolve_secrets(x, config),
                 "Default login method not set in 'VAULTR_AUTH_METHOD'")
  })
  withr::with_envvar(c(VAULTR_AUTH_METHOD = "token", VAULT_TOKEN = NA), {
    expect_error(resolve_secrets(x, config), "Vault token was not found")
  })
  withr::with_envvar(c(VAULTR_AUTH_METHOD = "token", VAULT_TOKEN = "fake"), {
    expect_error(resolve_secrets(x, config),
                 "Token login failed with error")
  })

  withr::with_envvar(c(VAULTR_AUTH_METHOD = "token", VAULT_TOKEN = srv$token), {
    expect_equal(resolve_secrets(x, config),
                 list(name = "alice", password = "ALICE"))
    expect_equal(resolve_secrets(unlist(x), config),
                 list(name = "alice", password = "ALICE"))
  })
})
