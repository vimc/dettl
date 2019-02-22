envir_read <- function(path) {
  filename <- path_envir_yml(path)
  if (file.exists(filename)) {
    ## TODO: check case VIMC-889
    dat <- yaml_read(filename)
    assert_named(dat, TRUE, basename(filename))
    n <- lengths(dat)
    nok <- n > 1L
    if (any(nok)) {
      stop(sprintf("Expected all elements of %s to be scalar (check %s)",
                   basename(filename),
                   paste(squote(names(dat)[nok]), collapse = ", ")))
    }
    vcapply(dat[n == 1], as.character)
  } else {
    NULL
  }
}

path_envir_yml <- function(path) {
  file.path(path, "dettl_envir.yml")
}
