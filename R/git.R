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
#' This is the directory contianing the .git files. Located using
#' \code{git rev-parse --git-dir}.
#'
#' @param dir The directory to locate the git project root from.
#' @return The git project root normalized, or throws an error if this can't
#' be located.
#'
#' @keywords internal
git_root_directory <- function(dir) {
  args <- c("rev-parse", "--absolute-git-dir")
  res <- git_run(args, root = dir)
  if (!res$success) {
    stop(sprintf(
      "Can't run import as can't locate git directory from path %s. %s", dir,
      "Import must be under version control to be run."))
  }
  ## Return just the path to dir containing the .git dir relative to dir
  withr::with_dir(dir, normalizePath(dirname(res$output)))
}
