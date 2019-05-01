setup_config <- function(db_driver = "RSQLite::SQLite",
                         vault_server = "https://example.com",
                         db_pw = "VAULT:/secret/users/readonly:password",
                         log_table = "data_import_log") {
  path <- tempfile()
  dir.create(path)
  filename <- file.path(path, "dettl_config.yml")
  cfg <- readLines("template_dettl_config/dettl_config.yml")
  cfg_server <- gsub("<vault_server>", vault_server, cfg, fixed = TRUE)
  cfg_server <- gsub("<driver>", db_driver, cfg_server, fixed = TRUE)
  cfg_server <- gsub("<db_pw>", db_pw, cfg_server, fixed = TRUE)
  cfg_server <- gsub("<log_table>", log_table, cfg_server, fixed = TRUE)
  writeLines(cfg_server, filename)
  path
}

setup_dettl_config <- function(load = "func: load",
                               rewrite_keys = "") {
  path <- setup_dettl("example_load_template", "db_config.yml")
  filename <- file.path(path, "example_load_template", "dettl.yml")
  cfg <- readLines(filename)
  cfg <- gsub("<load>", load, cfg, fixed = TRUE)
  cfg <- gsub("<rewrite_keys>", rewrite_keys, cfg, fixed = TRUE)
  cfg <- unlist(strsplit(cfg, "\n"))
  writeLines(cfg, filename)
  file.path(path, "example_load_template")
}

setup_dettl <- function(dir, db_config) {
  path <- tempfile()
  dir.create(path)
  file.copy(dir, path, recursive = TRUE)
  file.copy(db_config, path)
  path
}
