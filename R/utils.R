`%||%` <- function(a, b) {
  if (is.null(a)) b else a
}

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

## Originally in cyphr:
find_file_descend <- function(target, start = ".", limit = "/") {
  root <- normalizePath(limit, mustWork = TRUE)
  start <- normalizePath(start, mustWork = TRUE)

  f <- function(path) {
    if (file.exists(file.path(path, target))) {
      return(path)
    }
    if (normalizePath(path, mustWork = TRUE) == root) {
      return(NULL)
    }
    parent <- normalizePath(file.path(path, ".."))
    if (parent == path) {
      return(NULL)
    }
    Recall(parent)
  }
  ret <- f(start)
  if (!(is.null(ret))) {
    ret <- normalizePath(ret, mustWork = TRUE)
  }
  ret
}


is_windows <- function() {
  Sys.info()[["sysname"]] == "Windows"
}

is_linux <- function() {
  Sys.info()[["sysname"]] == "Linux"
}

set_names <- function(x, nms) {
  names(x) <- nms
  x
}

file_exists <- function(..., check_case = FALSE, workdir = NULL,
                        force_case_check = FALSE) {
  files <- c(...)
  if (!is.null(workdir)) {
    assert_scalar_character(workdir)
    owd <- setwd(workdir)
    on.exit(setwd(owd))
  }
  exists <- file.exists(files)

  if (check_case) {
    incorrect_case <- logical(length(files))
    if (!is_linux() || force_case_check) {
      incorrect_case[exists] <-
        !vlapply(files[exists], file_has_canonical_case)
      if (any(incorrect_case)) {
        correct <- vcapply(files[incorrect_case], file_canonical_case)
        names(correct) <- files[incorrect_case]
        attr(exists, "incorrect_case") <- incorrect_case
        attr(exists, "correct_case") <- correct
        exists[incorrect_case] <- FALSE
      }
    }
  }

  exists
}

squote <- function(x) {
  sprintf("'%s'", x)
}

vlapply <- function(X, FUN, ...) {
  vapply(X, FUN, logical(1), ...)
}

vcapply <- function(X, FUN, ...) {
  vapply(X, FUN, character(1), ...)
}

file_split_base <- function(filename, lowercase = FALSE) {
  path <- strsplit(filename, "[/\\\\]")[[1L]]
  if (!nzchar(path[[1]])) {
    base <- "/"
    path <- path[-1L]
    absolute <- TRUE
  } else if (grepl("^[A-Za-z]:", path[[1]])) {
    base <- paste0(path[[1L]], "/")
    path <- path[-1L]
    absolute <- TRUE
  } else {
    base <- "."
    absolute <- FALSE
  }
  if (lowercase) {
    path <- tolower(path)
  }
  list(path = path[nzchar(path)], base = base, absolute = absolute)
}

file_has_canonical_case <- function(filename) {
  dat <- file_split_base(filename)
  base <- dat$base
  absolute <- dat$absolute

  for (p in dat$path) {
    if (p %in% dir(base, all.files = TRUE)) {
      base <- paste(base, p, sep = if (absolute) "" else "/")
      absolute <- FALSE
    } else {
      return(FALSE)
    }
  }
  TRUE
}

## This one here behaves differently on unix because we could have
## files called Foo and foo next to each other (but not on
## windows/mac)
file_canonical_case <- function(filename) {
  dat <- file_split_base(filename, TRUE)
  base <- dat$base
  path <- dat$path
  absolute <- dat$absolute

  for (p in dat$path) {
    pos <- dir(base, all.files = TRUE)
    i <- match(p, tolower(pos))
    if (is.na(i)) {
      return(NA_character_)
    } else {
      base <- paste(base, pos[[i]], sep = if (absolute) "" else "/")
      absolute <- FALSE
    }
  }

  if (grepl("^\\./", base) && !grepl("^\\./", filename)) {
    base <- sub("^\\./", "", base)
  }
  base
}

file_copy <- function(..., overwrite = TRUE) {
  ok <- file.copy(..., overwrite = overwrite)
  if (any(!ok)) {
    stop("Error copying files")
  }
  ok
}
