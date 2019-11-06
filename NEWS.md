# dettl 0.0.8

* Allow `test_transform` to refer to `extracted_data` (VIMC-3108)

# dettl 0.0.7

* Users can configure if an import to a particular DB needs to be
confirmed. If so they are asked a yes/no question when running load step (VIMC-3007)

# dettl 0.0.6

* Allow upload to not specify serial PKs when they are not referenced
by any other data within the upload (VIMC-3022)

# dettl 0.0.5

* Expose automatic load function for use in custom load functions (VIMC-3015)

# dettl 0.0.4

* Use `gert` package instead of system calls to git (VIMC-3004)

# dettl 0.0.3

* Read foreign key constraints automatically from the database (VIMC-2859)
