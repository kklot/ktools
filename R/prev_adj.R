#' Adjusted prevalence using sensitivity and specificity
#' 
#' Using a modified Lew and Levy  to allow non-uniform prior on the prevalenc
#'
#' @param n sample size
#' @param x number of cases
#' @param alpha shape1 of beta distribution prior
#' @param beta shape2 of beta distribution prior
#' @param sens sensitivity of the test
#' @param spec specificity of the test
#' @export
p_adj <- function(n, x, alpha=3, beta=15, sens=.995, spec=.9) {
	# hist(rbeta(3000, 3, 15))
	fm = function(s1, s2) 
		integrate(function(x, shape1, shape2)
							x*dbeta(x, shape1, shape2), lower=1-spec, upper=sens, shape1=s1, shape2=s2)$value
	fn = function(s1, s2) 
		integrate(dbeta, lower=1-spec, upper=sens, shape1=s1, shape2=s2)$value
	di = fm(x+alpha, n-x+beta)/ fn(x+alpha, n-x+beta) 
	(di + spec - 1)/(sens + spec -1)
}
p_adj <- Vectorize(p_adj)              # 

#' Variance of adjusted prevalence using sensitivity and specificity
#'
#' using directly the formula not posterior distribution
#'
#' @param pi the adjusted prevalence
#' @param n sample size
#' @param spec specificity
#' @param sens sensitivity
var_ll <- function(pi, n, spec=.9, sens=.995) pi * (1 - pi) /(n*(sens-spec-1)^2)

