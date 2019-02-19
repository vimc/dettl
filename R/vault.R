#' Use configured vault to resolve secrets.
#'
#' This is copied from orderly.
#'
#' @param x List of properties some of which may be vault secrets of the form
#' VAULT:<path to secret>:<key to get>
#' @param config Cnfig containing details of the vault server to be accessed.
#'
#' @return List of properties with any secrets resolved secret.
#'
#' @keywords internal
#'
resolve_secrets <- function(x, config) {
  re <- "^VAULT:(.+):(.+)"
  if (is.list(x)) {
    i <- vlapply(x, function(el) is.character(el) && grepl(re, el))
    if (any(i)) {
      x[i] <- resolve_secrets(vcapply(x[i], identity), config)
    }
  } else {
    i <- grepl(re, x)
    if (any(i)) {
      loadNamespace("vaultr")
      vault <- withr::with_envvar(
        envir_read(config$path),
        vaultr::vault_client(login = TRUE, addr = config$vault_server))
      key <- unname(sub(re, "\\1", x[i]))
      field <- unname(sub(re, "\\2", x[i]))
      x[i] <- unname(Map(vault$read, key, field))
    }
  }
  x
}
