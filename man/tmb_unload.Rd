% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tmbutils.R
\name{tmb_unload}
\alias{tmb_unload}
\title{Unload loaded TMB dynamics}
\usage{
tmb_unload(x)

tmb_unload(x)
}
\arguments{
\item{x}{character name of the TMB file}

\item{name}{Name of the package, exact.}
}
\value{
nothing
}
\description{
when using `devtools` multiple times, multiple vesions of the same package
exist. Tried to unload with several others methods but none work for me
except this way.

help when rerun the code with unload included
}
