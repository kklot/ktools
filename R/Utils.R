#' Invert
#' 
#' inverting an number (I told you this package is trivial)
#' @param x R object
#' @export
invert <- function(x) {
    1/x
}
#' is sorted?
#' 
#' As title
#' 
#' @param x vector of integer
#' @export
is_sorted <- function(x) {
  all(diff(x) > 0) | all(diff(x) < 0)
}

#' Find consecutive (by one unit only)
#' 
#' As title
#' @param x vector of integer
#' @export
find_consecutive <- function(x) {
  if (!is_sorted) x <- sort(x)
  dd <- c(0, diff(x))
  which(dd==1)  
}

#' Remove consecutive (by one unit only)
#' 
#' When you want to remove/merge consecutive grouping to one, such as combining consecutive survey year to one. Is there a simpler way to do this?
#' 
#' @param x vector of integer
#' @export
remove_consecutive <- function(x, keep_first=TRUE) {
  if (!is_sorted) x <- sort(x)
  done <- FALSE
  while(!done) {
    id <- find_consecutive(x)
    if (keep_first) 
      x[id] <- x[id-1]
    else
      x[id-1] <- x[id]
    done <- length(find_consecutive(x)) == 0
  }
  x
}

#' Is negative
#' 
#' Just a replacement of `<`(0)
#' @param x
#' @export
is_negative <- function(x) {
    x < 0
}

#' shorthand ISO to country name from countrycode package
#' 
#' @export
iso2name <- function(x) {
  code <- ifelse(nchar(x)==2, 'iso2c', 'iso3c')
  countrycode::countrycode(x, code, 'country.name')
}


#' rownames to id
#' 
#' mainly used to extract coef. of named random effects in TMB, INLA
#' @export
row2id <- function(pattern, x) grep(pattern, rownames(x))


#' Range to sequence
#' 
#' give me a vector, I produces a continous sequence from the range
#' @export
range2seq <- function(x) eval(parse(text=paste0(range(x), collapse=':')))

#' Evaluate a text as expression
#' 
#' give me a vector, I produces a continous sequence from the range
#' @export
eval_text <- function(x) eval(parse(text=x))


getcoding_readstata13 <- function(data, var_name) {
  meta_obj <- getmeta_readstata13(data)
  a = meta_obj[meta_obj$variable==var_name, 'labels'] %>% strsplit('\\|') %>% unlist
  b = meta_obj[meta_obj$variable==var_name, 'values'] %>% strsplit('\\|') %>% unlist
  data.frame(labels=a, values=as.numeric(b))
}

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

list_name_to_column <- function(a_list, col_name="ele_name") {
   name <- names(a_list)
   lapply(seq_along(a_list), function(x) {
      oc_name <- colnames(a_list[[x]])
      o <- cbind(a_list[[x]], name[x]) 
      colnames(o) <- c(oc_name, col_name)
      o
    })
 }


#' Cut at zero
#' 
#' @export
cutat0 = function(x, n, mid=0) 
  c(seq(min(x), mid, length=n), seq(mid, max(x), length=n))[-n]

#' Get original value before scale
#' 
#' not covering all cases
#' 
#' @export
scale_inverse <- function(x) x*attr(x, "scaled:scale")+attr(x, "scaled:center")

#' Show data frame in browser
#' 
#' @export
browse <- function(x, nrow=66, ...) {
  x <- as.data.frame(x)
  plot(
    googleVis::gvisTable(
      x, 
      options = list(page='enable',
                     height="100%",
                     pageSize = nrow, 
                     showRowNumber=TRUE,...))
    )
}

#' @export 
show_colors <- function(x) {
  if (missing(x)) 
    plot(factor(palette()), col=palette())
  else
    plot(factor(x), col=x)
}

#' @export 
showPch <- function() {
    dev.new()
    plot(0:25, pch=0:25, col=1:25)
    text(0:25, labels=0:25, pos=3, xpd=T)
    text(0:25, labels=0:25, pos=3, xpd=T)
}

#' Auto sort the columns to plotable matrix
#' 
#' @export
image2 <- function(X, x, y, z, plot=1, ...) {
  X <- as.data.frame(X)[, c(x,y,z)]
  X <- X[order(X[,x], X[,y]), ]
  M <- as.matrix(tidyr::spread(X, y, z))
  opts <- list(...)
  if (plot) {
    if (length(opts)==0) 
      heatmap(M, Rowv = NA, Colv = NA)
    else
      do.call("heatmap", c(M, opts))
  } 
  else 
    M
}
# ggplot with some personal customizations
#' @export
ggplotk <- function(...) {
  require(ggplot2, quietly=TRUE)
  theme_set(theme_minimal())
  cls <- palette()
  p <- ggplot2::ggplot(...)
  vname <- gsub('.*\\((.*)\\)', "\\1", p$labels$col)
  n_cls <- max(length(unique(p$data[,vname])),
               nrow(unique(p$data[,vname])))
  if (length(palette()) < n_cls)
    cls <- colorRampPalette(cls)(n_cls)
  p + scale_colour_manual(values=unname(cls))
}
#' scale_to_length 
#' 
#' Scale a vector such that the sum is length vector
#' 
#' @export
scale_to_n <- function(x) x %>% multiply_by(length(x)) %>% divide_by(sum(x))

#' @export
as_numeric <- function(x) {
  if (is.character(x)) 
    return(as.numeric(as.factor(x)))
  else 
    return(as.numeric(x))
}

#' @export
cmc_to_year <- function(cmc) 1900 + floor( (cmc - 1) / 12 )

#' @export
cmc_to_month <- function(cmc) cmc - 12 * ( cmc_to_year(cmc) - 1900)

#' @export
text_center <- function(...) {
  text(mean(par("usr")[1:2]), mean(par("usr")[3:4]),...)
}
get_center <- function() {
  c(mean(par("usr")[1:2]), mean(par("usr")[3:4]))
}
#' s2n - String to numeric
#'
#' Converting string to ACSII number, to use with plotting parameter `pch`
#' @export
s2n <- function(x = "string") {
  if (!is.character(x)) stop("Enclosing the input in double quote!")
  num <- c(33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126)
  char <- c("!", "\"", "#", "$", "%", "&", "'", "(", ")", "*", "+", ",", "-", ".", "/", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", ":", ";", "<", "=", ">", "?", "@", "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z", "[", "\\", "]", "^", "_", "`", "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z", "{", "|", "}", "~")
  return(num[match(x, char)])
}

fibonaci <- function(len=10) {
  fibvals <- numeric(len)
  fibvals[1] <- 1
  fibvals[2] <- 1
  for (i in 3:len) { 
     fibvals[i] <- fibvals[i-1]+fibvals[i-2]
  }
  fibvals
}

#' AUC calculation
#'
#' Numerical integration
#' 
#' @export
AUC <- function(x, y, maxX=length(x)) {
  x <- x[0:maxX]
  y <- y[0:maxX]
  return(sum(diff(x) * (head(y,-1)+tail(y,-1)))/2)
}

# #' log 10 shortcut
# #'
# #' Shorten the log10() to l()
# #' @export
# l <- function(x) log10(x)

#' genSmooth
#'
#' Generate smoothed data dynamics
#' @export
genSmooth <- function(x, y, newx=NULL, logy=TRUE, method = 2, ...) {
    xData <- NULL
    xData$x <- newx
    if (logy) y <- log10(y)
    if (method == 1) {
      yfit <- loess(y ~ x,...)
      xData$y <- 10^predict(yfit, data.frame(x = newx))
    } else {
      yfit <- smooth.spline(x, y, all.knots = TRUE, ...)
      xData$y <- 10^predict(yfit, newx)$y
    }
    pointplot(x, y, pch = 16)
    lines(xData$x, log10(xData$y), col = 2, lwd = 2)
    return(as.data.frame(xData))
}
#' Not in
#'
#' Subseting not in
#' @export
`%notin%` <- function (x, table) is.na(match(x, table, nomatch=NA_integer_))
## ------------------------------------------------------------------------
#' Order data frame
#'
#' Order data frame by *one* colummn variable
#' @export
orderBy <- function(.data,index) as.data.frame(.data[order(.data[, index]), ])
## ------------------------------------------------------------------------
#' PFU 2 2 TCID50
#'
#' Convert Plaque Forming Unit to TCID50
#' @export
pfu2TCID50 <- function(x) -x*log(.5)
## ------------------------------------------------------------------------
#' Date to Rate
#'
#' Convert day to rate per day in log10 scale
#' @export
d2r <- function(x) log10(log(2)/x)
# --------------------------------------------------------------------------
#' Binding rows, auto set colnames
#' 
#' @export
rBind <- function(x, y,...) {
  colnames(y) <- colnames(x)
  rbind(x, y, ...)
}
