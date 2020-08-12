#' rownames to id
#' 
#' mainly used to extract coef. of named random effects in TMB, INLA
row2id <- function(pattern, x) grep(pattern, rownames(x))


#' Range to sequence
#' 
#' give me a vector, I produces a continous sequence from the range
range2seq <- function(x) eval(parse(text=paste0(range(x), collapse=':')))

#' Calculate Information criteria for TMB model
#' 
#' @param obj TMB object
#' @param n_post Number of posterior samples
#' @param pointwise Name of pointwise predictive density from your model
#' @param looic Report leave one out IC from `loo` package?
tmb_ICs <- function(obj, n_post=1000, pointwise='pwdens', looic=FALSE) {
  
  if (is.null(obj$env$random)) {
    joint_cov = sdreport(obj)$cov.fixed
  } else {
    joint_cov = as.matrix(solve(sdreport(obj,,,1)$jointPrecision))
  }
  post_sample = mvtnorm::rmvnorm(n_post, obj$env$last.par.best, joint_cov)

  # pointwise_predictive_density
  ppd = apply(post_sample, 1, function(x) obj$report(x)[[pointwise]])
  log_ppd = sum(log(rowMeans(ppd)))

  # DIC
  log_post = sum(log(obj$report(obj$env$last.par.best)[[pointwise]]))
  mean_log = mean(colSums(log(ppd)))

  # WAIC
  log_mean_post = log(rowMeans(ppd))
  mean_log_post = rowMeans(log(ppd))

  p_DIC   = 2 * (log_post - mean_log)
  p_WAIC1 = 2 * sum(log_mean_post - mean_log_post)
  p_WAIC2 = sum(apply(log(ppd), 1, var))

  # LOO-PSIS # n_post x N
  LOOIC = NULL
  if (looic) {
    LOOIC = loo::loo(t(log(ppd)))$looic
  }

  c(DIC   = -2 * log_post + 2 * p_DIC,
    WAIC1 = -2 * (log_ppd - p_WAIC1),
    WAIC2 = -2 * (log_ppd - p_WAIC2),
    LOOIC = LOOIC
  )
}


genQ <- function(n=10, order=2) {
    D <- diff(diag(n), diff = 2)
    t(D) %*% D
}

gen_inla_rw <- function(n=10, order=1, sd=1, seed=123) {
    Q = INLA:::inla.rw(n, order=order, scale.model=TRUE)
    constr = list(A = rbind(rep(1, n), c(scale(1:n))), e = rep(0, 2))
    INLA::inla.qsample(1, sd^-2 * (Q+Matrix::Diagonal(n, 1e-9)), constr=constr, seed=seed)
}


NullSpace <- function (A) {
  m <- dim(A)[1]; n <- dim(A)[2]
  ## QR factorization and rank detection
  QR <- base::qr.default(A)
  r <- QR$rank
  ## cases 2 to 4
  if ((r < min(m, n)) || (m < n)) {
    R <- QR$qr[1:r, , drop = FALSE]
    P <- QR$pivot
    F <- R[, (r + 1):n, drop = FALSE]
    I <- base::diag(1, n - r)
    B <- -1.0 * base::backsolve(R, F, r)
    Y <- base::rbind(B, I)
    X <- Y[base::order(P), , drop = FALSE]
    return(X)
    }
  ## case 1
  return(base::matrix(0, n, 1))
}

# hazard function for log-logistic distribution (parameterize as in INLA)
hz_llogis <- function (x, alpha = 8, lambda = 1/18) {
    num <- alpha * lambda
    den <- (lambda * x)^(1-alpha) + lambda * x
    num/den
}

mu_llogis <- function(alpha, lambda) { # convert to other
    (1 / lambda * pi * 1 / alpha) /(sin(pi / alpha))
}

mu_llogis2 <- function(alpha, lambda) { # convert to other
    pi / (alpha*lambda* sin(pi / alpha))
}

# density function for log-logistic distribution (parameterize as in INLA)
d_llogis <- function (x, alpha = 8, lambda = 1/18) {
    num <- alpha
    den <- x^(1+alpha)*lambda^alpha + x^(1-alpha)*lambda^(-alpha) + 2*x
    num/den
}
d_llogis2 <- function (x, alpha = 8, lambda = 1/18) {
    num <- alpha * lambda * (x * lambda)^(alpha-1)
    den <- ( (lambda*x)^alpha + 1 )^2
    num/den
}

# convert mean back to lambda
lambda_llogis <- function(mu, alpha) pi / (mu * alpha * sin(pi/alpha))


# Cumulative function for log-logistic distribution (parameterize as in INLA)
cdf_llogis <- function (x, alpha = 8, lambda = 1/18) {
    1 / ( 1 + (lambda * x)^(-alpha) )
}

logit <- function(p) log(p/(1-p))
invlogit <- function(x) 1/(1+exp(-x))
ldinvlogit <- function(x){v <- invlogit(x); log(v) + log(1-v)}

fwrite_bz2 <- function(x, y) {
    data.table::fwrite(x, y)
    system(paste0('bzip2 -f ', y))
}

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

efunc <- function(e) 'Error'

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

#' TMB fix parameters
#' 
#' @export
tmb_fixit <- function(par, fix_names) {
  for (i in names(par)) {
    if (i %in% fix_names)
      par[[i]][] <- NA
    else
      par[[i]] <- NULL
  }
  par %>% lapply(as.factor)
}

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
foo <- function() browser()

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

#' @export
brk <- function() {
  cat("Parent:\n", deparse(sys.calls()[[sys.nframe()-1]]), '\n')
  browser()
}

#' @export
remove <- function(pat='_$') rm(list=ls()[grep(pat, ls())])

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
# take_note()

#' AUC calculation
#'
#' Shorten the log10() to l()
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

#' #' genSmooth
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
#' wait function
#'
#' Add background and grid lines similar to ggplot.
#' @param bg (string) Background color Defaults to "gray90".
#' @param cols (string) Gridlines color  Defaults to "white".
#' @keywords grid
#' @export
#' @examples
#' wait()
wait <- function(wait.time = 2){
  now <- proc.time()[3]
  while(proc.time()[3] < (now + wait.time)) dum <- 0
}

#' @export
plott <- function(...) txtplot::txtplot(...) 


#' Binding rows, auto set colnames
#' 
#' @export
rBind <- function(x, y,...) {
  colnames(y) <- colnames(x)
  rbind(x, y, ...)
}
