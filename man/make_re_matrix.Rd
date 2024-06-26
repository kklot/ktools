% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tmbutils.R
\name{make_re_matrix}
\alias{make_re_matrix}
\title{Generate model matrix for random effect, such as AR model}
\usage{
make_re_matrix(x, lower, upper, space = 1)
}
\arguments{
\item{x}{the covariate that we wish to smooth}
}
\description{
the original value is saved a attributes
}
\details{
the original values is made unique and sorted
}
\examples{
make_re_matrix(sample(1:10, 7))
}
