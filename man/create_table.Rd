% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_table.R
\name{create_table}
\alias{create_table}
\title{Create a table describing the dataframe.}
\usage{
create_table(X, vars = NULL, names = NULL, strata = NULL)
}
\arguments{
\item{X}{data.frame containing variables}

\item{vars}{variables for which to create the table}

\item{names}{Optional vector containing names which will be entered in the table}

\item{strata}{Optional vector containing membership to different strata. Will add a column to the table for each stratum.}
}
\value{
Returns a matrix which containing a description of the variables.
}
\description{
Create a table describing the dataframe.
}
\examples{
   set.seed(1234)
   X <- data.frame(age = rnorm(1000), educ = sample(0:2, replace = T, size = 1000))
   strata <- sample(0:2, replace = T, size = 1000)
   create_table(X, names = c("Age", "Education"), strata = strata)
}
