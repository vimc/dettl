% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/transform_runner.R
\name{run_transform}
\alias{run_transform}
\title{Run transform step.}
\usage{
run_transform(transform, extracted_data, extract_passed, path)
}
\arguments{
\item{transform}{Transform function loaded from config.}

\item{extracted_data}{Data returned from the extract step.}

\item{extracted_passed}{TRUE if the extract stage tests passed successfully}
}
\value{
The transformed data.
}
\description{
Step is responsible for taking extracted data, running the configured
transform function on the data.
}
\keyword{internal}
