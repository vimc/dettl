# dettl

<!-- badges: start -->
[![Project Status: Concept – Minimal or no implementation has been done yet, or the repository is only intended to be a limited example, demo, or proof-of-concept.](https://www.repostatus.org/badges/latest/concept.svg)](https://www.repostatus.org/#concept)
[![R build status](https://github.com/vimc/dettl/workflows/R-CMD-check/badge.svg)](https://github.com/vimc/dettl/actions)
[![codecov.io](https://codecov.io/github/vimc/dettl/coverage.svg?branch=master)](https://codecov.io/github/vimc/dettl?branch=master)
[![CodeFactor](https://www.codefactor.io/repository/github/vimc/dettl/badge)](https://www.codefactor.io/repository/github/vimc/dettl)
<!-- badges: end -->

Data extract, transform, test and load.

Disinfect your workflow.

## Installation

Install via drat

```
drat:::add("vimc")
install.packages("dettl")
```

or using `remotes`

```
remotes::install_github("vimc/dettl")
```

## Using dettl

`dettl` requires a log table to exist in the target database for an import to be run. Run `dettl::dettl_create_log_table` to create the log table. This executes the relevant query at `inst/sql/` for create the log table. The name of the table can be changed and configured via the import `dettl_config.yml` but the fields must remain the same.

For details of how to write and run a data import using `dettl` see package vignettes.

## Using vault

Note that to use vault secrets in the `dettl_config.yml` to manage db access requires vault to be configured according to [vaultr vignette](https://vimc.github.io/vaultr/articles/vaultr.html) i.e. environmental variables need to be set which tell `vaultr` how it should authenticate users with access to the vault. 

To set these create a file called `envir.yml` in the root of the directory which sets the required properties for the chosen authentication method. You should probably create a `setup` script like [orderly](https://github.com/vimc/montagu-reports/blob/master/setup) to manage this.

### Tests which use the vault

This section is only relevant if you want to run the automated package tests.

Some testthat tests are configured which check the functionality of reading secrets from the vault. These tests are configured to use a test server which is controlled via `vaultr::vault_test_server()`. To set this up follow instructions in [vaultr packages vignette](https://vimc.github.io/vaultr/articles/packages.html).

## Running tests which rely on PostgreSQL database

Most tests run against a local SQLite database but some require a PostgreSQL database. These tests will be skipped by default when running locally. To enable them, run the `scripts/test_db_start.sh` script to run up a Postgres db within a docker container. When finished testing locally run `scripts/test_db_stop.sh` to stop the container.
