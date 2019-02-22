setup_config <- function(db_driver = "RSQLite::SQLite",
                         vault_server = "https://example.com",
                         db_pw = "VAULT:/secret/users/readonly:password") {
  path <- tempfile()
  dir.create(path)
  filename <- file.path(path, "db_config.yml")
  cfg <- readLines("template_db_config/db_config.yml")
  cfg_server <- gsub("<vault_server>", vault_server, cfg, fixed = TRUE)
  cfg_server <- gsub("<driver>", db_driver, cfg_server, fixed = TRUE)
  cfg_server <- gsub("<db_pw>", db_pw, cfg_server, fixed = TRUE)
  writeLines(cfg_server, filename)
  path
}
