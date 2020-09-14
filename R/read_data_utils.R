.IS_UNAR_EXIST = invisible(system('unar -v', intern=FALSE) == 0)
.UNAR_WARNINGS = "
  Using unzip but please install unar, unzip can't handle non-latin
  names, path, contents properly (try unziping Ivory Coast's MICS files).
  On Mac with Home brew (https://brew.sh/):
    brew install unar
    or go to https://theunarchiver.com/command-line
  On Ubuntu:
    sudo apt-get install unar"
.UNAR_WARNINGS_DID = FALSE
unar_warn <- function() {
  if (!.UNAR_WARNINGS_DID) warning(.UNAR_WARNINGS)
  .UNAR_WARNINGS_DID = TRUE
}
#' List zip file contents with unzip or unar (default)
#' 
#' Just to easy to remember the function name and alternate between unar and unzip
#' 
#' @param zipfile zip file
#' @param long Print more information about each file in the archive.
#' @param verylong Print all available information about each file in the archive.
#' @param password The password to use for decrypting protected archives.
#' @param encoding The encoding to use for filenames in the archive, when it is not known. If not specified, the program attempts to auto-detect the encoding used.
#' @param print_encoding Print the auto-detected encoding and the confidence factor after the file list
#' @param indexes Instead of specifying the files to list as filenames or wildcard patterns, specify them as indexes.
#' @export
list_zip <- function(zipfile, long=FALSE, verylong=FALSE, password, encoding, print_encoding=FALSE, indexes=FALSE, ...) {
    if (!.IS_UNAR_EXIST) {
      unar_warn()
      unzip(zipfile, list=TRUE,...)
    } else {
      cmd = paste("lsar")
      if (verylong) {
        long <- FALSE
        cmd <- paste(cmd, '-L')
      }
      if (long)
        cmd <- paste(cmd, '-l')
      if (!missing(password))
        cmd <- paste(cmd, '-p', password)
      if (!missing(encoding))
        cmd <- paste(cmd, '-p', encoding)
      if (print_encoding)
        cmd <- paste(cmd, '-pe')
      if (indexes)
        cmd <- paste(cmd, '-i')
      system(paste(cmd, shQuote(zipfile)), intern=TRUE)
    }
}

#' Unzip file with unar instead of unzip
#' 
#' Unzip file with unar instead of unzip
#' 
#' @param zipfile similar to \link[utils]{unzip}, put in \link[base]{shQuote} if there is space or special characters in the path, name.
#' @param files same as above parameter
#' @param overwrite similar to \link[utils]{unzip}
#' @param exdir similar to \link[utils]{unzip}
#' @param ... extra args to \link[utils]{unzip}
#' @param password unar only, zip password
#' @param force_directory Always create a containing directory for the contents of the unpacked archive? By default, a directory is created if there is more than one top-level file or folder.
#' @param no_directory Never create a containing directory for the contents of the unpacked archive (This should be used to have the same file path as list_zip.
#' @param encoding unar only, defined encoding, autodetect if not given
#' @param more_flags for unar, see man unar, not dupplicate what already here
#' @export
unar <- function(zipfile, files = NULL, overwrite = TRUE, exdir = ".", password, 
  encoding, force_directory = TRUE, no_directory = !force_directory, verbose=FALSE, more_flags, ...) {
    if (!.IS_UNAR_EXIST) {
      unar_warn()
      unzip(zipfile, files = files, overwrite = overwrite, exdir = exdir, ...)
    } else {
      if (exdir != ".")
        dir.create(exdir, 0)
      flags <- paste("-o", exdir)
      if (overwrite)
        flags <- paste(flags, "-f")
      if (force_directory)
        flags <- paste(flags, "-d")
      if (no_directory)
        flags <- paste(flags, "-D")
      if (!missing(password))
        flags <- paste(flags, "-p", password)
      if (!missing(encoding))
        flags <- paste(flags, "-e", encoding)
      if (!verbose)
        flags <- paste(flags, "-q")
      if (!missing(more_flags))
        flags <- paste(flags, more_flags)
      if (is.null(files))
        cmd <- paste("unar", flags, zipfile)
      else 
        cmd <- paste("unar", flags, zipfile, files)
      system(cmd, intern=TRUE)
    }
}

#' find filename matching regex in a zipped file
#' 
#' find filename matching regex in a zipped file
#' 
#' @param x character zip file path
#' @param pattern character regex pattern
#' @param basename boolean find in base name only (exclude path but with extensiion)
#' @param ... extra regex's args
#' @export
find_in_zip <- function(x, pattern, basename=FALSE, ...) {
  xpath <- where <- unzip(x, list=TRUE)$Name
  if (basename)
    where <- basename(where)
  found <- grep(pattern, where, ignore.case=TRUE, value=FALSE, useBytes=TRUE, ...)
  xpath[found]
}
#' Query variable and labels from data read with haven
#' 
#' Query variable and labels from data read with haven
#' 
#' @param d data
#' @param p character regex pattern for \link[base]{grep}
#' @param ... extra arguments to \link[base]{grep}
#' @export
query_name <- function(d, p, ...) {
  tab = label_table(d)
  position = grep(p, tab[, 'name'], 1,...)
  tab[position, ]
}

#' Query variable and labels from data read with haven
#' 
#' Query variable and labels from data read with haven
#' 
#' @param d data
#' @param p character regex pattern for \link[base]{grep}
#' @param ... extra arguments to \link[base]{grep}
#' @export
query_label <- function(d, p, ...) {
  tab = label_table(d)
  nmis = length(which(tab$label == ""))
  position = grep(p, tab[, 'label'], 1,...)
  if (nmis > 0)
    warnings('There is ', nmis, 'variables without label.\n')
  tab[position, ]
}

#' Generate lookup table for haven read spss/stata
#' 
#' Generate lookup table for haven read spss/stata
#' 
#' @param d data
#' @export
label_table <- function(d) {
  o <- lapply(names(d), function(x) {
      lab <- attributes(d[[x]])$label
      lab <- ifelse(is.null(lab), "", lab)
      data.table::data.table(name=x, label=lab)
  })
  data.table::rbindlist(o)
}


#' Read variable coding of a variable from stata file
#' 
#' Read variable coding of a variable from stata file
#' 
#' @param data data
#' @param var_name character name of variable
#' @export
getcoding_readstata13 <- function(data, var_name) {
  meta_obj <- getmeta_readstata13(data)
  a = meta_obj[meta_obj$variable==var_name, 'labels'] %>% strsplit('\\|') %>% unlist
  b = meta_obj[meta_obj$variable==var_name, 'values'] %>% strsplit('\\|') %>% unlist
  data.frame(labels=a, values=as.numeric(b))
}

#' Read meta data from stata file
#' 
#' This includes labels, coding values
#' 
#' @param data data
#' @export
getmeta_readstata13 <- function(data) {
    a = data.frame(
        variable = colnames(data),
        desc = attr(data, 'var.labels')
    )
    b = attr(data, 'label.table') %>% 
        sapply(function(x) {
            c(names(x) %>% paste0(collapse='|'), 
                unname(x) %>% paste0(collapse='|'))}) %>% 
        t %>% data.frame %>% tibble::rownames_to_column() %>% 
        set_colnames(c('variable', 'labels', 'values')) %>% 
        dplyr::mutate(variable = tolower(variable))
    ab <- a %>% dplyr::full_join(b, 'variable')
    class(ab) <- c(class(ab), "meta_readstata13")
    ab
}

#' Query data frame read with readstata13
#' 
#' @param d Data
#' @param x quoted regex such as '\\d+' to find digits
#' @param in_labels search in labels or in var names 
query_readstata13 <- function(d, x, in_labels=TRUE) {
  if (in_labels) {
    a = d %>% attr('var.labels') %>% grep(x,.,1,,1)
    b = d %>% attr('var.labels') %>% grep(x,.,1,,0)
    o = data.frame(
      DESC=a, ID=b, NAME=colnames(d)[b], 
      stringsAsFactors = FALSE
    )
  } else {
    o = colnames(d) %>% grep(x,.,1,,1)
  }
  return(o)
}
