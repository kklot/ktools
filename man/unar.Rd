% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_data_utils.R
\name{unar}
\alias{unar}
\title{Unzip file with unar instead of unzip}
\usage{
unar(
  zipfile,
  files = NULL,
  overwrite = TRUE,
  exdir = ".",
  password,
  encoding,
  force_directory = TRUE,
  no_directory = !force_directory,
  verbose = FALSE,
  more_flags,
  ...
)
}
\arguments{
\item{zipfile}{similar to \link[utils]{unzip}, put in \link[base]{shQuote} if there is space or special characters in the path, name.}

\item{files}{same as above parameter}

\item{overwrite}{similar to \link[utils]{unzip}}

\item{exdir}{similar to \link[utils]{unzip}}

\item{password}{unar only, zip password}

\item{encoding}{unar only, defined encoding, autodetect if not given}

\item{force_directory}{Always create a containing directory for the contents of the unpacked archive? By default, a directory is created if there is more than one top-level file or folder.}

\item{no_directory}{Never create a containing directory for the contents of the unpacked archive (This should be used to have the same file path as list_zip.}

\item{more_flags}{for unar, see man unar, not dupplicate what already here}

\item{...}{extra args to \link[utils]{unzip}}
}
\description{
Unzip file with unar instead of unzip
}
