% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/db.R
\name{dettl_db_args}
\alias{dettl_db_args}
\title{Get the DB args from config.}
\usage{
dettl_db_args(path, type = NULL)
}
\arguments{
\item{path}{Path to db config.}

\item{type}{The db type to get the args for, if null defaults to the first
configured database.}
}
\description{
Converts the configured DB driver to appropriate driver function and
map the args.
}
\keyword{internal}
