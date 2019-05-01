#' Save extracted data
#'
#' Saves any extracted data as separate sheet of an xlsx file.
#'
#' @param import The import object to save the extracted data for.
#' @param file File path at which to save the data.
#'
#' @export
#'
#' @examples
#' path <- prepare_test_import(
#'   system.file("examples", "person_information", package = "dettl"),
#'   system.file("examples", "dettl_config.yml", package = "dettl")
#' )
#' import <- dettl(file.path(path, "person_information"), "test")
#' run_import(import, "extract", dry_run = TRUE)
#' t <- tempfile()
#' save_extracted_data(import, t)
save_extracted_data <- function(import, file) {
  save_data(import, file, "extracted")
}

#' Save transformed data
#'
#' Saves any transformed data as separate sheet of an xlsx file.
#'
#' @param import The import object to save the transformed data for.
#' @param file File path at which to save the data.
#'
#' @export
#'
#' @examples
#' path <- prepare_test_import(
#'   system.file("examples", "person_information", package = "dettl"),
#'   system.file("examples", "dettl_config.yml", package = "dettl")
#' )
#' import <- dettl(file.path(path, "person_information"), "test")
#' run_import(import, c("extract", "transform"), dry_run = TRUE)
#' t <- tempfile()
#' save_transformed_data(import, t)
save_transformed_data <- function(import, file) {
  save_data(import, file, "transformed")
}

save_data <- function(import, file, stage) {
  if (!inherits(import, "DataImport")) {
    stop(sprintf("Can't save %s data for non-DataImport object.", stage))
  }
  dir.create(dirname(file), FALSE, TRUE)

  if (stage == "extracted") {
    data <- import$get_extracted_data()
  } else if (stage == "transformed") {
    data <- import$get_transformed_data()
  }

  if (is.null(data)) {
    stop(sprintf("Can't save %s data as stage has not been run.", stage))
  }
  writexl::write_xlsx(data, file)
  message(sprintf("Saved %s data to %s", stage, file))
  invisible(file)
}
