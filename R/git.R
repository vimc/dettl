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
git_repo_is_clean <- function(import_path) {
  root_dir <- git_root_directory(import_path)
  git_status(root_dir)$clean
}

#' Check git status
#'
#' Check that git status using \code{git status --porcelian}. Optionally
#' ignoring untracked files.
#'
#' @param root The git project root directory.
#' @return Result from git including output and indication of success.
#'
#' @keywords internal
git_status <- function(root = NULL) {
  args <- c("status", "--porcelain")
  res <- git_run(args, root = root, check = TRUE)
  res$clean <- length(res$output) == 0L
  res
}

#' Run a git system command
#'
#' Check that git status using \code{git status --porcelian}. Optionally
#' ignoring untracked files.
#'
#' @param args The args to be passed to git
#' @param root The git project root directory.
#' @param check Whether exit code should be checked. If exist code not
#' successful then throw an error.
#' @return Result from git including output and indication of success.
#'
#' @keywords internal
#'
#' @examples
#' dettl:::git_run(c("status", "--porcelain"))
git_run <- function(args, root = NULL, check = FALSE) {
  git <- sys_which("git")
  if (!is.null(root)) {
    args <- c("-C", root, args)
  }
  res <- system3(git, args)
  if (check && !res$success) {
    stop(sprintf("Error code %d running command:\n%s",
                 res$code, paste0("  > ", res$output, collapse = "\n")))
  }
  res
}

#' Locate the git project root directory
#'
#' From a subdirectory within a git project locate the root directory.
#' This is the directory contianing the .git files.
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
      "Import must be under version control to be run."))
  }
  root_dir
}

#' Get the git user.name from git dir
#'
#' From a path to a git controlled dir get the user.name. This will be the
#' global user.name or local if it has been overriden.
#'
#' @param path Path to the directory to get the git user.name for.
#' @return The git user.name.
#'
#' @keywords internal
git_user <- function(path) {
  git_config(path, "user.name")
}

#' Get the git user.email from git dir
#'
#' From a path to a git controlled dir get the user.email. This will be the
#' global user.email or local if it has been overriden.
#'
#' @param path Path to the directory to get the git user.email for.
#' @return The git user.email.
#'
#' @keywords internal
git_email <- function(path) {
  git_config(path, "user.email")
}

git_config <- function(path, field) {
  args <- c("config", field)
  res <- git_run(args, root = path, check = TRUE)
  res$output
}

#' Get the current git branch from path.
#'
#' From a path to a git controlled dir get the current active branch. This
#' throws an error if branch can't be retrieved.
#'
#' @param path Path to the directory to get the branch for.
#' @return The git branch or error if can't be found
#'
#' @keywords internal
git_branch <- function(path) {
  args <- c("symbolic-ref", "--short", "HEAD")
  res <- git_run(args, root = path, check = FALSE)
  if (!res$success){
    stop(sprintf(
      "Can't get current branch from path %s. %s", path,
      "Check repo is up to date and HEAD is not detatched."))
  }
  res$output
}
