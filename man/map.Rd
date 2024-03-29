% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotUtils.R
\name{map}
\alias{map}
\title{Leaflet map wrapper}
\usage{
map(
  .data,
  type = c("polygon", "point", "line"),
  ...,
  provider = "OpenStreetMap.DE"
)
}
\arguments{
\item{type}{which type to plot}

\item{...}{further customization}

\item{provider}{Provider base map}
}
\description{
For a quick plot with leafelt, can be piped to further processing
}
