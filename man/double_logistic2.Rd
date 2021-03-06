% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Utils.R
\name{double_logistic2}
\alias{double_logistic2}
\title{Double logistic with three bounds}
\usage{
double_logistic2(
  x = seq(0, 10, 0.01),
  bound1 = 1,
  bound2 = 0.1,
  bound3 = 0.5,
  rate1 = 2,
  rate2 = 5,
  midpoint1 = 3,
  midpoint2 = 7
)
}
\description{
Double logistic with three bounds
}
\examples{
plotl(double_logistic2())
}
\seealso{
\code{\link{double_logistic}}
}
