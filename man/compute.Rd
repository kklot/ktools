% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Utils.R
\name{compute}
\alias{compute}
\title{Do calculation, automatically converting type}
\usage{
compute(x, y, FUN = "+", ...)
}
\arguments{
\item{x}{scalar, vector}

\item{y}{scalar, vector}

\item{FUN}{function, default to add}

\item{...}{extra args to FUN}
}
\description{
If input is character then converting to factor then to numeric
}
