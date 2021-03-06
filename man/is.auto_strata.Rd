% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/S3_methods.R
\name{is.auto_strata}
\alias{is.auto_strata}
\title{Checks \code{auto_strata} class}
\usage{
is.auto_strata(object)
}
\arguments{
\item{object}{any R object}
}
\value{
Returns \code{TRUE} if its argument has \code{auto_strata} among its
  classes and \code{FALSE} otherwise.
}
\description{
Checks if the target object is an \code{auto_strata} object.
}
\examples{
dat <- make_sample_data()
a.strat <- auto_stratify(dat, "treat", outcome ~ X1 + X2)
is.auto_strata(a.strat) # returns TRUE
}
