build_git_demo <- function(example_dir = "example",
                           dettl_config = "dettl_config.yml") {
  path <- setup_dettl(example_dir, dettl_config)
  gert::git_init(path = path)
  gert::git_config_set(name = "user.email",
                       value = "email@example.com",
                       repo = path)

  gert::git_config_set(name = "user.name",
                       value = "dettl",
                       repo = path)

  writeLines(c("*.sqlite"), file.path(path, ".gitignore"))

  gert::git_add(files = "*", repo = path)
  # Working around gert bug, will file PR to fix
  author <- gert::git_signature_default(path)
  gert::git_commit(message = "initial-import", repo = path, author = author)
  stopifnot(git_repo_is_clean(path))

  path
}
