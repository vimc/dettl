#' Save data
#'
#' Saves any extracted and/or transformed data as separate sheet of an xlsx
#' file.
#'
#' @param import The import object to save the data for.
#' @param file File path at which to save the data.
#' @param stage The stage or stages to save. 'extract' and/or 'transform'
#'
#' @export
#'
#' @examples
#' path <- dettl::prepare_test_import(
#'   system.file("examples", "person_information", package = "dettl"),
#'   system.file("examples", "dettl_config.yml", package = "dettl")
#' )
#' import <- dettl::dettl(file.path(path, "person_information"), "test")
#' import$extract()
#' import$transform()
#' t <- tempfile()
#' dettl::dettl_save(import, t, "extract")
#' t2 <- tempfile()
#' dettl::dettl_save(import, t2, "transform")
#' t3 <- tempfile()
#' dettl::dettl_save(import, t3, c("extract", "transform"))
dettl_save <- function(import, file, stage) {
  if (!inherits(import, "DataImport")) {
    stop(sprintf("Can't save %s data for non-DataImport object.",
                 paste(stage, collapse = " and ")))
  }
  dir.create(dirname(file), FALSE, TRUE)

  if (all(stage == "extract")) {
    data <- import$get_extracted_data()
  } else if (all(stage == "transform")) {
    data <- import$get_transformed_data()
  } else if ("extract" %in% stage && "transform" %in% stage) {
    extract <- import$get_extracted_data()
    ## Data written to xlsx must have unique names
    names(extract) <- lapply(names(extract),
                             function(x) paste0("extracted_", x))
    transform <- import$get_transformed_data()
    names(transform) <- lapply(names(transform),
                               function(x) paste0("transformed_", x))
    data <- c(extract, transform)
  }

  if (is.null(data)) {
    stop(sprintf("Can't save %s data as stage has not been run.",
         paste(stage, collapse = ", ")))
  }
  writexl::write_xlsx(data, file)
  messages <- sprintf("Saved %s data to %s", stage, file)
  message(paste(messages, collapse = "\n"))
  invisible(file)
}
