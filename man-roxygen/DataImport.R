##' @section Methods:
##'
##' \describe{
##' \item{\code{extract}}{
##'   Run the extract stage of the data import
##'   \cr\emph{Usage:}\preformatted{extract()}
##' }
##' \item{\code{transform}}{
##'   Run the transform stage of the data import
##'   \cr\emph{Usage:}\preformatted{transform()}
##' }
##' \item{\code{load}}{
##'   Run the load stage of the data import
##'   \cr\emph{Usage:}\preformatted{load(comment = NULL, dry_run = FALSE, force = FALSE)}
##'
##'   \emph{Arguments:}
##'   \itemize{
##'     \item{\code{comment}:   Optional comment which will be persisted in the log of the import run in the database.
##'     }
##'
##'     \item{\code{dry_run}:   Whether to run in dry run mode. If TRUE then any database changes will be rolled back.
##'     }
##'
##'     \item{\code{force}:   If TRUE then checks that repo is up to date with git remote will be skipped.
##'     }
##'   }
##' }
##' \item{\code{get_extracted_data}}{
##'   Get the extracted data created by the extract step
##'   \cr\emph{Usage:}\preformatted{get_extracted_data()}
##' }
##' \item{\code{get_transformed_data}}{
##'   Get the transformed data created by the transform step
##'   \cr\emph{Usage:}\preformatted{get_transformed_data()}
##' }
##' }
