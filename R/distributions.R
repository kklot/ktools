#' Kullback Liebler
#' 
#' @param target target distribution density
#' @param estimate estimate distribution density that we wish to compare
#' @export
KLD = function(target, estimate) 
{
  if (length(target) !=  length(estimate)) stop('size is not equal')
  estimate[which(estimate < .Machine$double.xmin)] <- .Machine$double.xmin
  target = target / sum (target)
  estimate = estimate / sum (estimate)
  sum(target * log (target / estimate))
}


#' gumbel copulas prob
#' 
#' @param u1 dimension 1
#' @param u2 dimension 2
#' @param alpha copula parameter
#' @export
pgumbelCopula <- function(u1, u2, a, give_log=FALSE) {
    if  (a < 1) stop('a in [1:Inf]')
    l1  = -log(u1)
    l2  = -log(u2)
    t12 = l1^a + l2^a
    A   = 1/a
    cdf = exp(-t12^A)
    (if (give_log) log else identity)(cdf)
}
#' gumbel copulas density
#' 
#' @param u1 dimension 1
#' @param u2 dimension 2
#' @param alpha copula parameter
#' @export
dgumbelCopula <- function(u1, u2, a, give_log=FALSE) {
    if  (a < 1) stop('a in [1:Inf]')
    l1  = -log(u1)
    l2  = -log(u2)
    t12 = l1^a + l2^a
    A   = 1/a
    cdf = exp(-t12^A)
    tt  = (l1 * l2)^(a-1) * a / (u1 * u2)
    pdf = cdf * tt * ( t12^(2*A-2) / a  -  t12^(A - 2) * (A -1) )
    (if (give_log) log else identity)(pdf)
}
#' Frank copulas prob
#' 
#' @param u1 dimension 1
#' @param u2 dimension 2
#' @param alpha copula parameter
#' @export
pfrankCopula <- function(u1,  u2, alpha, log=FALSE) {
    e1 = exp(-alpha * u1)
    e2 = exp(-alpha * u2)
    t0 = exp(-alpha) - 1
    t1 = log((e1 - 1)/t0)
    t2 = log((e2 - 1)/t0)
    t3 = e2 * alpha / t0 / ((e2 - 1)/t0)
    t4 = e1 * alpha / t0 / ((e1 - 1)/t0)
    t5 = exp(-(-t1 - t2))
    tt = -1/alpha * log( 1 + t5 * t0 )
    if (log) return(log(tt))
    tt
}
#' Frank copulas density
#' 
#' @param u1 dimension 1
#' @param u2 dimension 2
#' @param alpha copula parameter
#' @export
dfrankCopula <- function(u1, u2, alpha, log=FALSE) {
    e1 = exp(-alpha * u1)
    e2 = exp(-alpha * u2)
    t0 = exp(-alpha) - 1
    t1 = log((e1 - 1)/t0)
    t2 = log((e2 - 1)/t0)
    t3 = e2 * alpha / t0 / ((e2 - 1)/t0)
    t4 = e1 * alpha / t0 / ((e1 - 1)/t0)
    t5 = exp(-(-t1 - t2))
    tt = -1/alpha * (
        t5 * t3 * t4 * t0 / (1 + t5 * t0) - 
        t5 * t4 * t0 * (t5 * t3 * t0) / (1 + t5 * t0)^2
    )
    if (log) return(log(tt))
    tt
}
#' Frank copulas parameter to kendall tau or spearman rho
#' @examples
#' corfrankCopula(1, 'spearman')
#' @export
corfrankCopula <- function(x, type=c('spearman', 'kendall')) {
    switch(type,
        kendall = 1 - 4/x * (4 - gsl::debye_1(x)), 
        spearman = 1 - 12/x * (gsl::debye_1(x) - gsl::debye_2(x))
    )
}
#' quantile function
#' 
#' @param scale scale
#' @param shape shape
#' @param skew skewness parameters
#' @export
qskewlogis <- function (q, lambda, p, gamma) 1/lambda * (-1 + q^(-1/gamma))^(-1/p)

#'  same as f_gllogisI
#' 
#' @param scale scale
#' @param shape shape
#' @param skew skewness parameters
#' @export
dskewlogis <- function(x, scale, shape, skew, log = FALSE) {
  term = (scale * x)^-shape
  o = (1/x) * skew * shape * term / (1 + term)^(skew+1)
	if (log)
		o = log(o)
	o
}

#'  same as F_gllogisI
#' 
#' @param scale scale
#' @param shape shape
#' @param skew skewness parameters
#' @export
pskewlogis <- function(q, scale, shape, skew) (1 + (scale*q)^-shape)^-skew

#' Sampling from skew logistic
#'  
#' @param scale scale
#' @param shape shape
#' @param skew skewness parameters
#' @export
r_glogisI <- function(n, scale=0.05, shape=7, skew=1) {
    u = runif(n)
    1/scale * ( u^(-skew^-1) - 1) ^(-shape^-1)
}
#' @export
rskewlogis <- r_glogisI

#' Compute the variance of skew loglogistic distribution
#' 
#' @param scale scale
#' @param shape shape
#' @param skew skewness parameters
#' @export
var_skew_llogis <- function(shape, skew) {
  shape^2 * (pi^2/6 + trigamma(skew))
}

#' Compute the mean of skew loglogistic distribution
#' 
#' @param scale scale
#' @param shape shape
#' @param skew skewness parameters
#' @export
mu_skew_llogis <- function(scale, shape, skew) {
  scale + shape * (digamma(1) - digamma(skew))
}

#' Generalized logistic type I density
#' 
#' @export
f_glogisI <- function(x, alpha, beta, gamma) {
  (gamma/beta) * exp((alpha-x)/beta) * (1 + exp((alpha-x)/beta))^(-gamma-1)
}
#' @export
dskewlogisx <- f_glogisI
#' Generalized logistic type I cumulative
#' 
#' @export
F_glogisI <- function(x, alpha, beta, gamma) {
  1 / (1 + exp((alpha-x)/beta)^gamma)
}
#' @export
pskewlogisx <- F_glogisI

#' Generalized logistic type I survival
#' 
#' @export
S_glogisI <- function(x, alpha, beta, gamma) {
  1 - F_glogisI(x, alpha, beta, gamma)
}

#' Generalized log-logistic type I density
#' 
#' @export
f_gllogisI <- function(t, lambda, p, gamma) {
  term = (lambda * t)^-p
  (1/t) * gamma * p * term / (1 + term)^(gamma+1)
}

#' Generalized log-logistic type I survival
#' 
#' @export
S_gllogisI <- function(t, lambda, p, gamma) {
  term = (lambda * t)^-p
  1 - 1 / (1 + term)^gamma;
}

#' Generalized log-logistic type I probability density
#' 
#' @export
F_gllogisI <- function(t, lambda, p, gamma) {
  (1 + (lambda*t)^-p)^-gamma
}

#' Generalized log-logistic type I probability density
#' 
#' @export
median_gllogisI <- function(lambda, p, gamma) {
  1/lambda * (-1 + 0.5^(-1/gamma))^(-1/p)
}

# hazard function for log-logistic distribution (parameterize as in INLA)
#' @export
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
#' @export
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
#' @export
cdf_llogis <- function (x, alpha = 8, lambda = 1/18) {
    1 / ( 1 + (lambda * x)^(-alpha) )
}

logit <- function(p) log(p/(1-p))
invlogit <- function(x) 1/(1+exp(-x))
ldinvlogit <- function(x){v <- invlogit(x); log(v) + log(1-v)}
