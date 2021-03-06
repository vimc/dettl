% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/automatic_load_order.R
\name{get_network_dependencies}
\alias{get_network_dependencies}
\title{Construct list of network dependencies for use in building upload order}
\usage{
get_network_dependencies(constraints, tables, non_empty_columns)
}
\arguments{
\item{constraints}{Table of constraints from db schema the important
columns are
constraint_table, constraints_column, referenced_table, referenced_column
where constraint_* is the table and column the constraint is on and the
referenced_* is the table and column the constraint references so e.g.
for postgres a constraint like
"constr_name" FOREIGN KEY (model_version) REFERENCES model_version(id)
on table burden_estimate_set would have a row like
constraint_table, constraints_column, referenced_table, referenced_column
burden_estimate_set, model_version, model_version, id}

\item{tables}{Names of tables being uploaded}

\item{non_empty_columns}{Names list of columns in tables which are non-empty
and so we must construct the network of fks respecting these present columns.
Other constraints can be ignored for working out the order.}
}
\value{
A list of table to dependencies
A | B, C
B | C
C | NA
Meaning B, C must exist before A can be added and C must exist before B
can be added and C has no dependencies
}
\description{
Construct list of network dependencies for use in building upload order
}
\keyword{internal}
