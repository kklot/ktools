#' MakeADFun safely terminated if there is a bound error
#' 
#' to prevent TMB crashing an R session. This should not be done with TMB in parallel mode
#' 
#' @param ... MakeADFun argments
#' 
#' @export
MakeADFunSafe <- function(...) {
  if (is_windows()) stop('not able to do parallel jobs on Windows')
  .n <- TMB::openmp()
  on.exit(TMB::openmp(.n))
  TMB::openmp(1)
  o <- parallel::mcparallel(TMB::MakeADFun(...))
  parallel::mccollect(o)[[1]]
}

#' Crash testing an expression
#' 
#' Only on Unix
#' 
#' @param x this will be quoted with rlang so anything
#' 
#' @export
crash_test <- function(x) {
  if (is_windows()) stop('not able to do parallel jobs on Windows')
  t = rlang::enquo(x)
  t = rlang::quo(parallel::mcparallel(!!t))
  t = rlang::eval_tidy(t)
  parallel::mccollect(t)
}

#' Compile TMB with ktools header
#' 
#' Add ktools.hpp to compile flags
#' 
#' @param user_flags extra flag to compile
#' @export
kompile <- function(..., user_flags, verbose=FALSE) {
  kfile <- system.file('include', 'ktools.hpp', package='ktools')
  kheader <- paste0("-I", dirname(kfile))
  if (!verbose)
    kheader <- paste(kheader, '-Wno-macro-redefined -Wno-unused-variable -Wno-unused-function -Wno-unused-local-typedefs -Wno-unknown-pragmas -Wno-c++11-inline-namespace')
  if (!missing(user_flags))
    kheader <- paste(kheader, user_flags)
  compile(..., flags=kheader)
}

#' precision to sd
#' 
#' standard deviation to precision
#' @param x precision
#' @export
prec2sd <- function(x) {
    x^-.5
}

#' sd to precision
#' 
#' standard deviation to precision
#' @param x standard deviation
#' @export
sd2prec <- function(x) {
    1/x^2
}

#' Calculate Information criteria for TMB model
#' 
#' @param obj TMB object
#' @param n_post Number of posterior samples
#' @param pointwise Name of pointwise predictive density from your model
#' @param looic Report leave one out IC from `loo` package?
#' @export
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

#' Generate random walk precision structure matrix
#' 
#' @export
genR <- function(n=10, order=2, scale=1) {
    D <- diff(diag(n), diff = order)
    Q <- t(D) %*% D # == crossprod
    if (!scale) return(Q)
    Q_pert = Q + Matrix::Diagonal(n) * max(diag(Q)) * sqrt(.Machine$double.eps)
    INLA::inla.scale.model(Q_pert)
}

#' @export
gen_inla_rw <- function(n=10, order=1, sd=1, seed=123) {
    Q = INLA:::inla.rw(n, order=order, scale.model=TRUE)
    constr = list(A = rbind(rep(1, n), c(scale(1:n))), e = rep(0, 2))
    INLA::inla.qsample(1, sd^-2 * (Q+Matrix::Diagonal(n, 1e-9)), constr=constr, seed=seed)
}

#' Simulated random walk
#' 
#' @export
gen_rw <- function(n, order=2, sig=0.1) {
  D <- diff(diag(n), diff = order) # differences matrix 
  x_i = rnorm(n - order, sd = sig)
  c(t(D) %*% solve( D %*% t(D) ) %*% x_i)
}

#' @export
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

#' TMB fix parameters
#' @param par initial set of parameter
#' @param fix_names names of parameters to be fixed (not fitting)
#' @return TMB's format for map argument
#' @seealso \code{\link[ktools]{get_report}}.
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

#' Get values of summary(\code{\link[TMB]{sd_report}}) by name
#' 
#' Get values of summary(\code{\link[TMB]{sd_report}}) by name
#' 
#' @param x object
#' @param name regex of parameter's name
#' @param se get the standard error as well
#' @export
get_report <- function(x, name='.*', se = TRUE) {
  pick_rows <- row2id(name, x)
  cols <- ifelse(se, c(1, 2), 1)
  x[row2id(name, x), cols]
}
