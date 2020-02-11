check_valid_mode <- function(mode) {
  valid_modes <- c("append", "create")
  if (!(mode %in% valid_modes)) {
    stop(sprintf('Invalid mode - mode must be one of %s got "%s".',
                 paste(valid_modes, collapse = ", "), mode))
  }
  invisible(TRUE)
}

allow_create_table <- function(mode) {
  allow <- FALSE
  if (mode == "create") {
    allow <- TRUE
  }
  allow
}

get_auto_load_function <- function(mode) {
  auto_load_function <- dettl_auto_load
  if (mode == "create") {
    auto_load_function <- dettl_auto_load_create
  }
  auto_load_function
}
