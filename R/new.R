#' Create new directory and templated code for new dettl process.
#'
#' @param name The name of the project directory to be created. Should be human
#' readable and meaningful. Any non a-z,0-9,_ characters will be stripped
#' and replaced with _s. Directory name will be prepended with created date.
#'
#' @export
#'
#' @examples
#' dettl::dettl_new("test import")
dettl_new <- function(name) {
  clean_name <- paste(Sys.Date(), gsub("[^[:alnum:]]", "_" ,name),
                sep = "_")
  dir_created <- dir.create(clean_name, showWarnings = FALSE)
  if (!dir_created) {
    stop(sprintf(
      "Can't create new dettl process, failed to create directory with name %s from name %s. One may already exist.",
      clean_name, name))
  }
  file_copy(
    list.files(
      system.file("template", package = "dettl"),
      include.dirs = TRUE,
      full.names = TRUE
    ),
    clean_name,
    recursive = TRUE, overwrite = FALSE
  )
  clean_name
}
