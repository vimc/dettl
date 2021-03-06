setup_config <- function(db_driver = "RSQLite::SQLite",
                         vault_server = "https://example.com",
                         db_pw = "VAULT:/secret/users/readonly:password",
                         log_table = "data_import_log",
                         confirm = FALSE,
                         require_branch = NULL) {
  path <- temp_file()
  dir.create(path)
  filename <- file.path(path, "dettl_config.yml")
  cfg <- readLines("template_dettl_config/dettl_config.yml")
  cfg_server <- gsub("<vault_server>", vault_server, cfg, fixed = TRUE)
  cfg_server <- gsub("<driver>", db_driver, cfg_server, fixed = TRUE)
  cfg_server <- gsub("<db_pw>", db_pw, cfg_server, fixed = TRUE)
  cfg_server <- gsub("<log_table>", log_table, cfg_server, fixed = TRUE)
  cfg_server <- gsub("<confirm>", confirm, cfg_server, fixed = TRUE)
  if (!is.null(require_branch)) {
    cfg_server <- gsub("<require_branch>",
                       paste0("require_branch: ", require_branch),
                       cfg_server, fixed = TRUE)
  } else {
    cfg_server <- gsub("<require_branch>", "", cfg_server, fixed = TRUE)
  }
  writeLines(cfg_server, filename)
  path
}

setup_dettl_config <- function(load = "func: load", dettl = "") {
  path <- setup_dettl("example_load_template", "dettl_config.yml")
  filename <- file.path(path, "example_load_template", "dettl.yml")
  cfg <- readLines(filename)
  cfg <- gsub("<load>", load, cfg, fixed = TRUE)
  cfg <- gsub("<dettl>", dettl, cfg, fixed = TRUE)
  cfg <- unlist(strsplit(cfg, "\n"))
  writeLines(cfg, filename)
  file.path(path, "example_load_template")
}
