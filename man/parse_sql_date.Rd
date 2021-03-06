% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/db.R
\name{parse_sql_date}
\alias{parse_sql_date}
\title{Parse date into POSIXct UTC from SQL db.}
\usage{
parse_sql_date(con, date)
}
\arguments{
\item{con}{Active db connection.}

\item{date}{The date.}
}
\value{
The parsed date
}
\description{
Parses the date according to the SQL driver used. Expects that if
driver is Postgres this will already be POSIXct otherwise if SQLite
driver converts to POSIXct.
}
\keyword{internal}
