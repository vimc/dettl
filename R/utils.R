yaml_read <- function(filename) {
  catch_yaml <- function(e) {
    stop(sprintf("while reading '%s'\n%s", filename, e$message),
         call. = FALSE)
  }
  tryCatch(yaml::yaml.load_file(filename),
           error = catch_yaml)
}

check_fields <- function(x, name, required, optional) {
  msg <- setdiff(required, names(x))
  if (length(msg) > 0L) {
    stop(sprintf("Fields missing from %s: %s",
                 name, paste(msg, collapse = ", ")))
  }
  extra <- setdiff(names(x), c(required, optional))
  if (length(extra) > 0L) {
    stop(sprintf("Unknown fields in %s: %s",
                 name, paste(extra, collapse = ", ")))
  }
}

check_symbol_from_str <- function(str, name) {
  assert_scalar_character(str)
  dat <- strsplit(str, "::", fixed = TRUE)[[1L]]
  if (length(dat) != 2) {
    stop(sprintf("Expected fully qualified name for %s", name))
  }
  dat
}

is_absolute_path <- function(path) {
  grepl("^(/|[A-Z][a-z]:)", path)
}

is_relative_path <- function(path) {
  !is_absolute_path(path)
}
