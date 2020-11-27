#'  same as f_gllogisI
#' 
#' @param scale scale
#' @param shape shape
#' @param skew skewness parameters
#' @export
dskewlogis <- function(x, scale, shape, skew) {
  term = (scale * x)^-shape
  (1/x) * skew * shape * term / (1 + term)^(skew+1)
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
