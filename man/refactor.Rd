% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_data_utils.R
\name{refactor}
\alias{refactor}
\title{Help to refactor the factor easier}
\usage{
refactor(x, new_position = c())
}
\arguments{
\item{x}{a character/factor vector}

\item{new_position}{a integer vector specify what do you want the refactor
order look like, e.g., from 1, 2, 3 to 3, 1, 2}
}
\description{
Help to refactor the factor easier
}
\examples{
x <- LETTERS[1:5]
refactor(x, c(5, 1:4))
}
