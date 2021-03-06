% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/automatic_load_order.R
\name{get_non_empty_columns}
\alias{get_non_empty_columns}
\title{Get non-empty columns}
\usage{
get_non_empty_columns(data)
}
\arguments{
\item{data}{Lnames list of data frames representing db tables}
}
\value{
Pairs of table to non-empty columns as a list
}
\description{
These are the columns which contain any data and so we cannot ignore fk
constraints on these columns
}
\keyword{internal}
