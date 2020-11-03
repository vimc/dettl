#!/usr/bin/env Rscript

## Connect to postgres db

con <- DBI::dbConnect(RPostgres::Postgres(),
                      dbname = "dettl_test_db",
                      user = "postgres",
                      host = "localhost",
                      password = "password")
DBI::dbDisconnect(con)

## Connect to vault
#vaultr::vault_resolve_secrets()
