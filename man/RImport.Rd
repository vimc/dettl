% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/r_import.R
\name{RImport}
\alias{RImport}
\title{Manage R based data import.}
\description{
Manage R based data import.

Manage R based data import.
}
\details{
This object should not be initialised directly. Use \code{\link{dettl}} to
create the object.

Import can be run by working with import object returned by
\code{\link{dettl}} or by running top-level functions. Run the import by
working with this object if you want to step through the import process
stage by stage and inspect the data after each stage.
}
\examples{
path <- dettl:::prepare_test_import(
  system.file("examples", "person_information", package = "dettl"),
  system.file("examples", "dettl_config.yml", package = "dettl"))
import_path <- file.path(path, "person_information")

import <- dettl::dettl(import_path, db_name = "test")
import$extract()
import$transform()
import$load()

}
\section{Super class}{
\code{\link[dettl:Import]{dettl::Import}} -> \code{RImport}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-reload}{\code{RImport$reload()}}
\item \href{#method-read_config}{\code{RImport$read_config()}}
\item \href{#method-get_extracted_data}{\code{RImport$get_extracted_data()}}
\item \href{#method-get_transformed_data}{\code{RImport$get_transformed_data()}}
\item \href{#method-extract}{\code{RImport$extract()}}
\item \href{#method-transform}{\code{RImport$transform()}}
\item \href{#method-pre_modify_checks}{\code{RImport$pre_modify_checks()}}
\item \href{#method-run_import}{\code{RImport$run_import()}}
}
}
\if{html}{
\out{<details ><summary>Inherited methods</summary>}
\itemize{
\item \out{<span class="pkg-link" data-pkg="dettl" data-topic="Import" data-id="begin_transaction">}\href{../../dettl/html/Import.html#method-begin_transaction}{\code{dettl::Import$begin_transaction()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="dettl" data-topic="Import" data-id="commit_transaction">}\href{../../dettl/html/Import.html#method-commit_transaction}{\code{dettl::Import$commit_transaction()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="dettl" data-topic="Import" data-id="format">}\href{../../dettl/html/Import.html#method-format}{\code{dettl::Import$format()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="dettl" data-topic="Import" data-id="get_connection">}\href{../../dettl/html/Import.html#method-get_connection}{\code{dettl::Import$get_connection()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="dettl" data-topic="Import" data-id="get_log_table">}\href{../../dettl/html/Import.html#method-get_log_table}{\code{dettl::Import$get_log_table()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="dettl" data-topic="Import" data-id="help">}\href{../../dettl/html/Import.html#method-help}{\code{dettl::Import$help()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="dettl" data-topic="Import" data-id="initialize">}\href{../../dettl/html/Import.html#method-initialize}{\code{dettl::Import$initialize()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="dettl" data-topic="Import" data-id="load">}\href{../../dettl/html/Import.html#method-load}{\code{dettl::Import$load()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="dettl" data-topic="Import" data-id="rollback_transaction">}\href{../../dettl/html/Import.html#method-rollback_transaction}{\code{dettl::Import$rollback_transaction()}}\out{</span>}
}
\out{</details>}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-reload"></a>}}
\if{latex}{\out{\hypertarget{method-reload}{}}}
\subsection{Method \code{reload()}}{
Reload the objects sources to refresh source code or repair a broken
Postgres connection.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{RImport$reload()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-read_config"></a>}}
\if{latex}{\out{\hypertarget{method-read_config}{}}}
\subsection{Method \code{read_config()}}{
Read and parse config from path.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{RImport$read_config()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-get_extracted_data"></a>}}
\if{latex}{\out{\hypertarget{method-get_extracted_data}{}}}
\subsection{Method \code{get_extracted_data()}}{
Get the extracted data created by the extract step
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{RImport$get_extracted_data()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
The extracted data
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-get_transformed_data"></a>}}
\if{latex}{\out{\hypertarget{method-get_transformed_data}{}}}
\subsection{Method \code{get_transformed_data()}}{
Get the transformed data created by the transform step
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{RImport$get_transformed_data()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
The transformed data
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-extract"></a>}}
\if{latex}{\out{\hypertarget{method-extract}{}}}
\subsection{Method \code{extract()}}{
Run the extract stage of the data import
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{RImport$extract()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-transform"></a>}}
\if{latex}{\out{\hypertarget{method-transform}{}}}
\subsection{Method \code{transform()}}{
Run the transform stage of the data import
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{RImport$transform()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-pre_modify_checks"></a>}}
\if{latex}{\out{\hypertarget{method-pre_modify_checks}{}}}
\subsection{Method \code{pre_modify_checks()}}{
Run suite of checks to verify that db can be modified
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{RImport$pre_modify_checks(dry_run, allow_dirty_git)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{dry_run}}{Whether to run in dry run mode. If TRUE then any database
changes will be rolled back. Defaults to FALSE.}

\item{\code{allow_dirty_git}}{If TRUE then skips check that the import is up to
date with remote git repo. FALSE by default.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-run_import"></a>}}
\if{latex}{\out{\hypertarget{method-run_import}{}}}
\subsection{Method \code{run_import()}}{
Run multiple stages of the data import
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{RImport$run_import(
  comment = NULL,
  dry_run = FALSE,
  allow_dirty_git = FALSE,
  stage = c("extract", "transform"),
  save = FALSE
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{comment}}{Optional comment to be written to db log table when
import is run.}

\item{\code{dry_run}}{If TRUE then any changes to the database will be rolled
back.}

\item{\code{allow_dirty_git}}{If TRUE then skips check that the import is up
to date}

\item{\code{stage}}{The stage or stages of the import to be run.}

\item{\code{save}}{Path and name to save data from each stage at, if TRUE then
will save to a tempfile.}
}
\if{html}{\out{</div>}}
}
}
}
