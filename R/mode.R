check_valid_mode <- function(mode) {
  valid_modes <- c("append", "create", "sql")
  mode <- mode %||% "append"
  if (!(mode %in% valid_modes)) {
    stop(sprintf('Invalid mode - mode must be one of %s got "%s".',
                 paste(valid_modes, collapse = ", "), mode))
  }
  mode
}

allow_create_table <- function(mode) {
  mode == "create"
}

get_auto_load_function <- function(mode) {
  switch(mode,
         create = dettl_auto_load_create,
         append = dettl_auto_load)
}
