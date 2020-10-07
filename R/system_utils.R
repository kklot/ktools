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
.ktools_OLD_WD <- '~'
#' replicate some features of bash cd
#' 
#' replicate some features of bash cd such as go backs to previous working directory with cd('-')
#' 
#' @param x x
#' @export
cd <- function(to='~') {
    .ktools_OLD_WD <- getwd()
    if (to=="-")
      to <- .ktools_OLD_WD
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