#' findInterval and return factor with label 
#'
#' instead of numeric vector like the base version
#'
#' @export
findInterval2 <- function(x, vec, factor = TRUE, ...) {
    require(dplyr)
    y <- findInterval(x, vec, ...)
    width <- nchar(as.character(max(vec)))
    o <- bind_cols(x = x, y = y) %>%
        group_by(y) %>%
        transmute(
            a = floor(min(x)), z = floor(max(x)),
            z = if_else(a == z, z+1, z),
            a = formatC(a, flag = "0", width = width),
            z = formatC(z, flag = "0", width = width),
            lab = paste(a, z, sep = "-")
        ) %>%
        ungroup()
    if (factor) o <- pull(o, lab) else o <- pull(o, y)
    o
}

#' Convert to snake case from package snakecase
#'
#' @export
snaking <- snakecase::to_snake_case

#' scale to defined range
#'
#' @export
scale_to_range <- function(x, min, max) {
  #       (b-a)(x - min)
  # f(x) = --------------  + a
  #           max - min
  (max-min) * (x-min(x, na.rm=TRUE)) / (max(x, na.rm=TRUE)-min(x, na.rm=TRUE)) + min
}

#' Check if system is Windows
#' @export
is_windows <- function () (tolower(.Platform$OS.type) == "windows")

#' Find R binary depending on systems
#' 
#' @export
R_binary <- function () {
  R_exe <- ifelse (is_windows(), "R.exe", "R")
  return(file.path(R.home("bin"), R_exe))
}
#' Find text match regex with grep with some tailored options
#' 
#' Overidding is OK
#' 
#' @param ... grep's args
#' @export
grept <- function(pattern, x, ignore.case=1, useBytes=TRUE, value=TRUE,...) {
    defaultpar <- as.list(environment())
    # callpar <- as.list(match.call())[-1]
    wantpar <- join_list(defaultpar, list(...),,0)
    do.call('grep', wantpar)
}

#' Join elements of list a from list b
#' 
#' Join elements of list a from list b
#' 
#' @param of_a list to replace
#' @param from_b list with elements to take
#' @param add_new add new elements if not existed
#' @param replace TRUE replace elements with the same name
#' @export
join_list <- function(of_a, from_b, add_new=TRUE, replace=TRUE) {
    anames <- names(of_a)
    bnames <- names(from_b)
    shared_names <- intersect(anames, bnames)
    new_names <- bnames[which(!bnames %in% anames)]
    if (length(shared_names) > 1)
      message(length(shared_names), " elements with the same name, default to ignore")
    if (replace)
      if (add_new)
        donames <- c(shared_names, new_names)
      else
        donames <- shared_names
    else
      if (add_new)
        donames <- new_names
      else
        return(of_a)
    for (e in donames) of_a[[e]] <- from_b[[e]]
    of_a
}

#' Remove text from strings with gsub
#' 
#' Remove text from strings with gsub
#' 
#' @param x strings
#' @param p pattern
#' @export
remove_text <- function(x, p,...) {
    x <- gsub(p, '', x, ...)
}

#' replicate some features of bash cd
#' 
#' replicate some features of bash cd such as go backs to previous working directory with cd('-')
#' 
#' @param x x
#' @export
cd <- function(to='~') {
    if (to=="-") {
      to <- getOption("MY_LAST_WD")
      if (is.null(to)) {
        options("MY_LAST_WD"=getwd())
      }
    }
    options("MY_LAST_WD"=getwd())
    message("Moved from: ", getOption("MY_LAST_WD"))
    message(" to ", to, "\n")
    setwd(to)
}
#' Kill R session
#' 
#' Kill R session
#' 
#' @export
kill <- function(x) {
    message("Bye then!")
    quit('no')
} 

#' Open file with system's program (on MAC)
#' 
#' Description
#' @param x path to file
#' @export
open_file <- function(x) {
    system(paste('open', x))
}
#' Write bzipped data file
#' 
#' using bzip and data.table
#' @param x data file to write as csv
#' @export
fwrite_bz2 <- function(x, y, quote = TRUE) {
    y <- path.expand(y)
    data.table::fwrite(x, y)
    if (quote) y <- shQuote(y)
    system2('bzip2', c('-f', y))
}

#' Write a note to file
#' 
#' Useful for taking note in loop
#' 
#' @export
take_note <- function(text="text", to="Rnote", thisfilename=NULL,
                      home=FALSE, wd = !home, time_stamp=TRUE)
{
  to <- paste0(if (home) "~" else getwd(),
               format(Sys.time(), "/%Y_%b_%d_"), to, ".md")
  if (time_stamp)
    paste0('echo ', '"',
           format(Sys.time(), "%H:%M:%S"), '"', ' >> ', to) %>% system
  if (!is.null(thisfilename))
    paste0('echo ', '"in ', thisfilename, '"', ' >> ', to) %>% system
  paste0('echo ', '"', '\t- ', text, '"', " >> ", to) %>% system
}

#' wait function
#'
#' Add waiting time before evaluate an expression
#' 
#' @param wait.time in seconds
#' @keywords time
#' @export
wait <- function(wait.time = 2){
  now <- proc.time()[3]
  while(proc.time()[3] < (now + wait.time)) dum <- 0
}

#' @export
brk <- function() {
  cat("Parent:\n", deparse(sys.calls()[[sys.nframe()-1]]), '\n')
  browser()
}

#' @export
remove <- function(pat='_$') rm(list=ls()[grep(pat, ls())])

#' @export 
foo <- function() browser()

#' To use in in tryCatch
#' 
#' @export
efunc <- function(e) 'Error'

#' lsSize
#'
#' List top (default 10) objects by size
#' @export
lsSize <- function(x = 10) {
  # List top largest objects in R global environments
  temp <- NULL
  for (i in ls(envir = .GlobalEnv)) {
    temp <- rbind(temp, cbind(i, format(object.size(get(i)), units="Mb") ))
  }
  temp[,2] <- substr(temp[,2], 1, nchar(temp[,2])-3)
  temp <- as.data.frame(temp, stringsAsFactors=0)
  temp[,2] <- as.numeric(temp[,2])
  temp <- temp[order(temp[, 2], decreasing=TRUE), ]
  names(temp) <- c("name", "size(Mb)")
  return(head(temp, x))
  head(temp)
}
