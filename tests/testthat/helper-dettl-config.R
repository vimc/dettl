setup_config <- function(vault_server) {
  path <- tempfile()
  dir.create(path)
  filename <- file.path(path, "db_config.yml")
  cfg <- readLines("template_db_config/db_config.yml")
  cfg_server <- gsub("<vault_server>", vault_server, cfg, fixed = TRUE)
  writeLines(cfg_server, filename)
  path
}
