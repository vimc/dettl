#' Check if git repo is clean
#'
#' Check that git repo is up to date with remote by checking
#' \code{git status --porcelian}. If import_path is not a git root directory
#' this will search parents of the import_path for the git root
#'
#' @param import_path Path to the directory of the dettl import to check.
#' @return True if repo is up to date. False otherwise.
#'
#' @keywords internal
git_repo_is_clean <- function(root = ".") {
  nrow(gert::git_status(repo = git_root_directory(root)))==0
}

#' Locate the git project root directory
#'
#' From a subdirectory within a git project locate the root directory.
#' This is the directory containing the .git files.
#'
#' @param dir The directory to locate the git project root from.
#' @return The git project root normalized, or throws an error if this can't
#' be located.
#'
#' @keywords internal
git_root_directory <- function(dir) {
  root_dir <- find_file_descend(".git", dir)
  if (is.null(root_dir)) {
    stop(sprintf(
      "Can't run import as can't locate git directory from path %s. %s", dir,
      "Import must be under version control to be run."
    ))
  }
  root_dir
}


#' Get the git user.name from git dir
#'
#' From a path to a git controlled dir get the user.name. This will be the
#' global user.name or local if it has been overridden.
#'
#' @param path Path to the directory to get the git user.name for.
#' @return The git user.name.
#'
#' @keywords internal
git_user <- function(path = ".") {
  git_config(path, "user.name")
}

#' Get the git user.email from git dir
#'
#' From a path to a git controlled dir get the user.email. This will be the
#' global user.email or local if it has been overridden.
#'
#' @param path Path to the directory to get the git user.email for.
#' @return The git user.email.
#'
#' @keywords internal
git_email <- function(path = ".") {
  git_config(path, "user.email")
}

git_config <- function(path = ".", field) {
  gitres <- gert::git_config(repo = git_root_directory(path))
  gitres$value[gitres$name == field]
}

#' Get the current git branch from path.
#'
#' From a path to a git controlled dir get the current active branch. This
#' throws an error if branch can't be retrieved.
#'
#' @param path Path to the directory to get the branch for.
#' @return The git branch or error if can't be found.
#'
#' @keywords internal
git_branch <- function(path) {
  gert::git_info(repo = git_root_directory(path))$shorthand
}

#' Get the full hash of the current git HEAD
#'
#' Get SHA-1 hash of HEAD using \code{git rev-parse HEAD}
#'
#' @param path Path to the directory to get the hash for.
#' @return Hash of HEAD.
#'
#' @keywords internal
git_hash <- function(path) {
  gert::git_info(repo = git_root_directory(path))$commit
}
