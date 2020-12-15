#' Open output of a function in a new text file (default application)
#' 
#' @param x name of a function
#' @examples
#' fn_to_file(lm)
#' @export
screen_to_file <- function(x) {
  file <- tempfile(fileext = ".R")
  name <- tools::file_path_sans_ext(file)
  writeLines(capture.output(x), file)
  open_file(file)
}
#' CMC from R date
#' 
#' @param x r object
#' @export
date_to_cmc <- function(x) {
  12 * (as.numeric(strftime(x, "%Y")) - 1900) + as.numeric(strftime(x, "%m"))
}

#' STATA long date format to R date
#' 
#' @param x r object
#' @export
STATA_date_to_date <- function(x) {
    x + structure(-3653, class = "Date")
}
#' Month year to cmc
#' 
#' @param x r object
#' @export
month_year_to_cmc <- function(month, year) 12*(year-1900) + month
#' Range label like 2019-20
#' 
#' @param x r object
#' @export
range_label <- function(x, start=3, end=4) {
  if (min(x, na.rm=TRUE)==max(x, na.rm=TRUE)) 
    return(as.character(min(x, na.rm=TRUE)))
  paste0(min(x, na.rm=TRUE), '-', substr(max(x, na.rm=TRUE), start, end))
}

#' Pager view similar to more
#' 
#' @param x r object
#' @export
more <- function(x) {
  # https://stackoverflow.com/a/3506450
  file <- tempfile()
  sink(file); on.exit(sink())
  print(x)
  file.show(file, delete.file = T)
}
#' View table as html
#' 
#' for, e.g, copy to Word editors keeping most of the format
#' 
#' @param code out of knitr::kable(format='html') for 
#' @export
table_as_html <- function(code, ...) {
  # dots <- modifyList(list(...), list(format='html'))
  # if (!inherits(code, 'knitr_kable')) {
    # require(knitr)
    # code <- do.call('kable', c(code, dots))
    # code <- do.call('kable', c(code, format='html'))
  # }
  file <- tempfile(fileext = ".html")
  name <- tools::file_path_sans_ext(file)
  writeLines(code, file)
  open_file(file)
}

#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

#' @importFrom magrittr %$%
#' @export
magrittr::`%$%`

#' @importFrom magrittr %<>%
#' @export
magrittr::`%<>%`

#' Convenient create vector of character without the need to quote
#' 
#' Convenient create vector of character without the need to quote, e.g. instead
#' of typing \code{c("a", "b", "c")} just write \code{char(a, b, c)}
#' 
#' 
#' @param ... list of character separate by comma
#' @export
char <- function(...) as.character(substitute(...()))

#' table that take pipe input
#' 
#' table that take pipe input
#' 
#' @param .data pipe or data
#' @param ... table's args
#' @export
bang <- function(.data, ...) {
  dots <- substitute(...())
  do.call('table', napply(  dots, eval, envir=.data))
}


#' format cross-IQR by groups (>2) for publishing
#' 
#' format cross-table for publishing, only kruskal wallis/bonferonni atm
#' @param .data pipe or data
#' @param ... table's args
#' @export
mustats_3plus <- function(.data, ...) {
  dots <- substitute(...())
  .o <- capture.output(o <- dunn.test::dunn.test(
    eval(dots[[1]], .data), eval(dots[[2]], .data), 
    method='bonferroni'))
  sample_size <- aggregate(eval(dots[[1]], .data), list(eval(dots[[2]], .data)), length)
  names <- paste(sample_size[, 1], bracket(sample_size[, 2]))
  sds  <- aggregate(eval(dots[[1]], .data), list(eval(dots[[2]], .data)), 
    quantile, prob=c(0.5, 0.25, 0.75), na.rm=TRUE) %>% format(1, 4)  
  .o <- paste0(sds[[2]], " [", sds[[3]], "-", sds[[4]], "]") %>% 
    stats::setNames(names)
  sig <- which(o$P.adjusted < 0.05)
  sig <- paste(format_pvalue(o$P.adjusted[sig]), bracket(o$comparisons[sig]), collapse='; ')
  if (length(sig)==0) sig <- ''
  t(c(.o, sig, 'bonferroni'))
}

#' format cross-table for publishing
#' 
#' format cross-table for publishing, only Fisher/chiquare here
#' 
#' @param row row variable
#' @param col column (outcome) variable
#' @export
tabstat <- function(.data, ...) {
  dots <- substitute(...())
  .o <- capture.output(o <- 
    suppressWarnings(
      gmodels::CrossTable(
        x=eval(dots[[1]], .data), 
        y=eval(dots[[2]], .data), 
        2, 3, 
        prop.r=TRUE, 
        prop.c=FALSE, 
        prop.t=FALSE, 
        prop.chisq=FALSE, 
        expected=TRUE, 
        chisq=TRUE, 
        fisher=TRUE, 
        missing.include=FALSE)
    )
  )
  o$t[] <- paste(format(o$t), bracket(format(round(o$prop.r, 2))))
  isFisher <- length(which(o$chisq$expected < 5))/length(o$chisq$expected) > .2
  p <- if (isFisher) o$fisher$p.value else o$chisq$p.value
  o <- cbind(o$t, c(format_pvalue(p), rep('', nrow(o$t)-1) ))
  colnames(o)[ncol(o)] <- 'p-value'
  tname <- "Chi-square"
  if (isFisher) tname <- 'Fisher'
  o <- cbind(o, c(tname, rep('', nrow(o)-1) ))
  colnames(o)[ncol(o)] <- 'test'
  if ('knit' %in% names(dots)) {
    if (eval(dots[['knit']])) {
      return(knitr::kable(o))
    }
  }
  o
}

#' format p-value for publishing
#' 
#' format p-value for publishing
#' 
#' @param x computed p-value
#' @export
format_pvalue <- function(x) {
  if (x < 0.001) return('< 0.001*')
  if (x < 0.01) return('< 0.01*')
  if (x > 0.1) return('> 0.1')
  format(round(x, 2))
}
format_pvalue <- Vectorize(format_pvalue)

#' dev.new with named width and height
#' 
#' dev.new with named width and height
#' 
#' @param width width
#' @param height height
#' @export
g <- function(width=7, height=5, ...) {
  dev.off()
  do.call('dev.new', modifyList(as.list(environment()), list(...)))
}

.extra_ISOA3 <- c(
  'Spratly Islands' = 'VNM',
  Kosovo = 'XKX'
)

# https://en.wikipedia.org/wiki/Sub-Saharan_Africa
.UN_SSA = c("Angola", "Benin", "Botswana", "Burkina Faso", "Burundi", "Cameroon", "Cape Verde", "Central African Republic", "Chad", "Comoros", "Democratic Republic of the Congo", "Djibouti", "Equatorial Guinea", "Eritrea", "Ethiopia", "Gabon", "Gambia", "Ghana", "Guinea", "Guinea Bissau", "Ivory Coast", "Kenya", "Lesotho", "Liberia", "Madagascar", "Malawi", "Mali", "Mauritania", "Mauritius", "Mozambique", "Namibia", "Niger", "Nigeria", "Republic of the Congo", "Rwanda", "São Tomé and Príncipe", "Senegal", "Seychelles", "Sierra Leone", "Somalia", "South Africa", "South Sudan", "Sudan", "Swaziland", "Tanzania", "Togo", "Uganda", "Zambia", "Zimbabwe")

#' Print ggplot off-screen
#' 
#' Print ggplot off-screen
#' 
#' @param gg ggplot object
#' @param name file name including path without extenstion
#' @param type pdf, png,...
#' @return nothing
#' @export
quartz_off <- function(gg, name='Rplot', width=7, height=5, type='pdf', open=FALSE,...) {
    filename <- paste0(name,'.',type)
    quartz(width=width, height=height,type=type, file=filename,...)
    print(gg)
    dev.off()
    if (open) open_file(filename)
}

#' quantile 95\% with name
#' 
#' quantile 95\% with name
#' 
#' @param x x
#' @return .025. .5, and .975 quantile with name lo, up, and med
#' @export
quantile95 <- function(x,...) {
  q = quantile(x, probs=c(.025, .5, .975),...) 
  names(q) <- c('lo', 'med', 'up')
  q
}

#' Not in = out
#' 
#' Not in = out
#' @export
`%out%` <- function (x, table) !(x %in% table)

#' TMB compile and load
#' 
#' TMB compile and load
#' 
#' @param x model character
#' @return dll
#' @export
tmb_compile_and_load <- function(code) {
  # from Jeff
  file <- tempfile(fileext = ".cpp")
  name <- tools::file_path_sans_ext(file)
  writeLines(code, file)
  TMB::compile(file)
  dyn.load(TMB::dynlib(name))
  basename(name)
}
#' Square
#' 
#' why not when we have square root
#' 
#' @param x R object
#' @export
square <- function(x) x^2

#' Generate widths for geom_tile with unevenly space x-axis
#' 
#' Calculate the widths needed to fill the white space when we have unevenly spaced x-axis value
#' 
#' @param x x-axis vector to plot
#' @return weights use in \code{\link[ggplot2]{geom_tile}}
#' @examples
#' \dontrun{
#' df <- data.frame(
#'   x = rep(c(2, 5, 7, 9, 12), 2),
#'   y = rep(c(1, 2), each = 5),
#'   z = factor(rep(1:5, each = 2))
#' )
#' w = gen_widths(df$x)
#' ggplot(df, aes(x, y)) +geom_tile(aes(fill = z, width=w), colour = "white")
#' }
#' @export
gen_widths <- function(x) {
    xx = sort(unique(x))
    wo = min(diff(xx))
    xl = xx - wo/2; xu = xx + wo/2
    yy = c(xl[-1] - xu[-length(xu)], 0)
    wo+(yy*2)
}

#' Today date
#' 
#' get today date in optinally date format
#' 
#' @param as_date R's date format
#' @export
today <- function(as_date=TRUE) {
  td = format(Sys.time(), '%Y-%m-%d')
  if(as_date)
    return(as.Date(td))
  td
}

#' Do calculation, automatically converting type
#' 
#' If input is character then converting to factor then to numeric
#' 
#' @param FUN function, default to add
#' @param x scalar, vector
#' @param y scalar, vector
#' @param ... extra args to FUN
#' @export
compute <- function(x, y, FUN = '+', ...) {
    FUN <- match.fun(FUN)
    x <- as_numeric(x)
    y <- as_numeric(y)
    FUN(x, y, ...)
}
#' @seealso compute
#' @export
add_to <- compute

#' Double logistic function
#' 
#' Double logistic function  have two periods each with a logistic shape, either one of them will be decreasing and the other increases.
#' 
#' @param x time horizon
#' @param bound1 the boundary of function (this will equals the function value at time zero)
#' @param bound2 the second boundary, if bound1 > bound2 the function increase first then decrease, and vice versa.
#' @param rate1 rate of change of first period, control the sharpness of changes
#' @param rate2 rate of change of second period, control the sharpness of changes
#' @param midpoint1 midpoint where the chance occurs in the first period
#' @param midpoint2 midpoint where the chance occurs in the second period
#' @return vector of length x
#' @seealso \code{\link{logistic}}
#' @references 
#' @note 
#' @author
#' @examples
#' plotl(double_logistic(bound1=0.1, bound2=0.8))
#' plotl(double_logistic(bound1=0.8, bound2=0.1))
#' 
#' @export
double_logistic <- function(x = seq(0, 10, 0.01), bound1 = 0, bound2= 1, rate1 = 2, rate2 = 5, midpoint1 = 3, midpoint2=7) {
  if (midpoint1 > max(x) | midpoint2 > max(x) | midpoint1 < min(x) | midpoint2 < min(x) | midpoint1 > midpoint2)
    stop('midpoints not in range!')
  t1 = 1 / (1 + exp(-rate1*(x - midpoint1)))
  t2 = 1 / (1 + exp( rate2*(x - midpoint2)))
  bound1 + (bound2-bound1) * ((t1 + t2) - 1)
}

#' Double logistic with three bounds
#' @seealso \code{\link{double_logistic}}
#' @examples
#' plotl(double_logistic2())
#' @export
double_logistic2 <- function(x = seq(0, 10, 0.01), bound1 = 1, bound2= 0.1, bound3 = 0.5,  rate1 = 2, rate2 = 5, midpoint1 = 3, midpoint2=7) {
  if (midpoint1 > max(x) | midpoint2 > max(x) | midpoint1 < min(x) | midpoint2 < min(x) | midpoint1 > midpoint2)
    stop('midpoints not in range!')
  t1 = 1 / (1 + exp(rate1*(x - midpoint1)))
  t2 = 1 / (1 + exp(-rate2*(x - midpoint2)))
  bound1 + (bound1-bound2) * (t1 - 1) + (bound3 - bound2) * t2
}

#' Logistic shape generator
#' 
#' Logistic shape generator
#' 
#' @param time time horizon
#' @param initial value at time zero
#' @param midpoint the time the function changes
#' @param percent_reduction in [0, 1], percentage reduction compare to the initial value after midpoint of the time horizon, so the end value equals initial x percent_reduction
#' @param shape control how sharp the transition from initial value to the end value
#' @export
logistic <- function(time=seq(0, 1, 0.01), initial=1, percent_reduction=0.5, midpoint=0.5, shape=1) {
  initial * (1 - percent_reduction) + initial * percent_reduction / (1 + (time/midpoint)^shape)
}


#' Generate precision matrix for spatial model from matrix of adjacency
#' 
#' Generate precision matrix for spatial model from matrix of adjacency
#' 
#' @param x matrix of connectivity with 1s denote connected nodes and zeros otherwise.
#' @export
adj_to_precision <- function(x) {
    x <- -x
    diag(x) <- 0
    diag(x) <- -rowSums(x)
    x
}

#' Remove NA/Inf from objects
#' 
#' Remove NA/Inf from objects
#' 
#' @param x R object
#' @export
nan_rm <- function(x) {
    x[!is.na(x) & is.finite(x)]
}

#' Number of unique elements in a vector
#' 
#' to type less
#' @param x vector
#' @export
n_unique <- function(x) {
    length(unique(x))
}
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
  if (!is_sorted(x)) x <- sort(x)
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
  if (!is_sorted(x)) x <- sort(x)
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
#' @param x x
#' @export
is_negative <- function(x) {
    x < 0
}

#' shorthand ISO to country name from countrycode package with some custom match
#' 
#' @export
iso2name <- function(x,...) {
  code <- ifelse(nchar(x)==2, 'iso2c', 'iso3c')
  countrycode::countrycode(x, code, 'country.name', custom_match=.extra_ISOA3,...)
}
iso2name <- Vectorize(iso2name)

#' shorthand country name to ISO from countrycode package with some custom match
#' 
#' @export
name2iso <- function(x, version=3,...) {
  code <- paste0('iso', version, 'c')
  countrycode::countrycode(x, 'country.name', code, custom_match=.extra_ISOA3,...)
}
name2iso <- Vectorize(name2iso)


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
eval_text <- Vectorize(eval_text)

#' Add nested list name to nested data column
#' 
#' Add nested list name to nested data column
#' 
#' @param a_list list contains nested data frame
#' @export
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
