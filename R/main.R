main_parse_args <- function(args = commandArgs(TRUE)) {
  usage <- "Usage:
  dettl [options] <path>

Options:
  --db-name=NAME     Name of the database to use
  --comment=COMMENT  Comment to add with the import
  --dry-run          Do the dry run only
  --force            Allow dirty git
  --root=PATH        Path to dettl root"

  res <- docopt_parse(usage, args)
  list(root = res[["root"]],
       args = list(path = res[["path"]],
                   db_name = res[["db_name"]],
                   comment = res[["comment"]],
                   dry_run = res[["dry_run"]],
                   force = res[["force"]]))
}


main <- function(args = commandArgs(TRUE)) {
  dat <- main_parse_args(args)
  if (!is.null(dat$root)) {
    owd <- setwd(dat$root)
    on.exit(setwd(owd))
  }
  do.call(dettl_run_load, dat$args)
}

docopt_parse <- function(usage, args) {
  dat <- docopt::docopt(usage, args)
  names(dat) <- gsub("-", "_", names(dat), fixed = TRUE)
  dat
}
