% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helper_testing.R
\name{add_fk_data}
\alias{add_fk_data}
\title{Add tables with foreign key constraints for testing}
\usage{
add_fk_data(con, schema = "public")
}
\arguments{
\item{con}{Connection to DB to add tables with foreign key constraints}
}
\description{
Create some tables with foreign key constraints and add some simple data
to them for testing.
}
\keyword{internal}
