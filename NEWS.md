# dettl 0.0.10

* Add "mode" option to dettl config, by default mode is "append". If use "create" then import can only create new tables and not append to existing ones. (VIMC-3426)

# dettl 0.0.9

* The import object's sources can be reloaded with `$reload()`, allowing refreshing of source code and repairing a Postgres connection with a failed transaction (VIMC-3035, partly fixes VIMC-3154)

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
