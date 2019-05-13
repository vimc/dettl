## SQL

This directory contains SQL scripts to create tables required by `dettl`. There are 2 subdirectories, one for SQLite and one for PostgreSQL dialects. Each directory should contain dialect specific versions of the same scripts. Scripts are:

* create_log_table.sql - Creates `dettl_import_log` table for storing a log each time an import is run.
