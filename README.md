# dettl

[![Travis-CI Build Status](https://travis-ci.org/vimc/dettl.svg?branch=master)](https://travis-ci.org/vimc/dettl)
[![codecov.io](https://codecov.io/github/vimc/dettl/coverage.svg?branch=master)](https://codecov.io/github/vimc/dettl?branch=master)

Data extract, transform, test and load.

Disinfect your workflow.

## Using vault

Note that to use vault secrets in the `db_config.yml` to manage db access requires vault to be configured according to [vaultr vignette](https://vimc.github.io/vaultr/articles/vaultr.html) i.e. environmental variables need to be set which tell `vaultr` how it should authenticate users with access to the vault. 

To set these create a file called `envir.yml` in the root of the directory which sets the required properties for the chosen authentication method. You should probably create a `setup` script like [orderly](https://github.com/vimc/montagu-reports/blob/master/setup) to manage this.

### Tests which use the vault

This section is only relevant if you want to run the automated package tests.

Some testthat tests are configured which check the functionality of reading secrets from the vault. These tests are configured to use a test server which is controlled via `vaultr::vault_test_server()`. For this to work you need to download the vault binary and specify the path to the binary by an environmental variable. 

In your `.Rprofile` set the path and port variables
```R
Sys.setenv("VAULTR_TEST_SERVER_BIN_PATH" = "<path to download binary to>")
Sys.setenv("VAULTR_TEST_SERVER_PORT" = 18200)
```

To download the binary run:
```R
Sys.setenv("NOT_CRAN", "true")
Sys.setenv("VAULTR_TEST_SERVER_INSTALL" = "true")
vaultr::vault_test_server_install()
Sys.unsetenv("NOT_CRAN")
Sys.unsetenv("VAULTR_TEST_SERVER_INSTALL")
```

Tests which use `vaultr::vault_test_server()` will now work. In addition note that these tests will be skipped unless run in an environment where the environmental variable `NOT_CRAN` is set to `true`. This environmental variable is set when using RStudio Build -> Test Package to test the package.
