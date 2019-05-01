build_git_demo <- function(example_dir = "example") {
  path <- setup_dettl(example_dir)
  git_run("init", root = path)
  git_run(c("config", "user.email", "email@example.com"), root = path,
          check = TRUE)
  git_run(c("config", "user.name", "dettl"), root = path, check = TRUE)
  writeLines(c("*.sqlite"), file.path(path, ".gitignore"))
  git_run(c("add", "."), root = path, check = TRUE)
  git_run(c("commit", "-m", "'initial-import'"), root = path, check = TRUE)
  stopifnot(git_repo_is_clean(path))

  path
}
