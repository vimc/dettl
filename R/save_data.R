#' Save extracted data
#'
#' Saves any extracted data as separate sheet of an xlsx file.
#'
#' @param import The import object to save the extracted data for.
#' @param file Optional file to save the data to, defaults to creating file in
#' current working directory called extracted_data.xlsx
#'
#' @export
#'
#' @examples
#' import <- new_import(system.file("examples/person_information", package = "dettl"), "test")
#' run_import(import, "extract")
#' t <- tempfile()
#' save_extracted_data(import, t)
save_extracted_data <- function(import, file = "extracted_data.xlsx") {
  save_data(import, file, "extracted")
}

#' Save transformed data
#'
#' Saves any transformed data as separate sheet of an xlsx file.
#'
#' @param import The import object to save the transformed data for.
#' @param file Optional file to save the data to, defaults to creating file in
#' current working directory called transformed_data.xlsx
#'
#' @export
#'
#' @examples
#' import <- new_import(system.file("examples/person_information", package = "dettl"), "test")
#' run_import(import, c("extract", "transform))
#' t <- tempfile()
#' save_transformed_data(import, t)
save_transformed_data <- function(import, file = "transformed_data.xlsx") {
  save_data(import, file, "transformed")
}

save_data <- function(import, file, stage) {
  if (!inherits(import, "DataImport")) {
    stop(sprintf("Can't save %s data for non-DataImport object.", stage))
  }
  if (!file.exists(file)) {
    file.create(file)
  }

  if (stage == "extracted") {
    data <- import$get_extracted_data()
  } else if (stage == "transformed") {
    data <- import$get_transformed_data()
  }

  if (is.null(data)) {
    stop(sprintf("Can't save %s data as stage has not been run.", stage))
  }
  writexl::write_xlsx(data, file)
  invisible(file)
}
