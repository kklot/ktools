#' Fractional polynomial 
#'
#' @export
fracpoly <- function(kx,  ky) {
  if (any(kx==0)) kx <- kx + 1
  if (any(ky==0)) ky <- log1p(ky) else  ky <- log(ky)
  # Range of powers
  ps <- c(-2,-1,-0.5,0,0.5,1,2,3)
  # Find best combination
  for (i1 in 1:8) {
    for (i2 in i1:8) {
      if (ps[i1] == 0) term1 <- log(kx) else term1 <- kx^ps[i1]
      if ( (ps[i1] == 0) & (ps[i2] == 0) ) term2 <- log(kx)^2 
      else {
        if (i2 == i1) term2 <- kx^ps[i1]*log(kx) 
          else {
            if (ps[i2]==0) term2 <- log(kx) 
              else term2 <- kx^ps[i2]
        }
      }
      fit <- glm(ky ~ term1 + term2)
      aic <- AIC(fit)
      # cat(i1,i2,aic,"\n")
      if (i1*i2 == 1) aicmin <- c(i1,i2,aic) else {
        if (aic < aicmin[3]) aicmin <- c(i1,i2,aic)
      }
    }
  }
  # aicmin
  i1 = aicmin[1]
  i2 = aicmin[2]
  # Print the best
  cat("Best powers",ps[i1],ps[i2],aic,"\n")
  # Build final terms
  if (ps[i1] == 0) term1=log(kx) else term1=kx^ps[i1]
  if ((ps[i1] == 0) & (ps[i2] == 0)) term2=log(kx)^2 else {
    if (i2 == i1) term2=kx^ps[i1]*log(kx) else { 
      if (ps[i2]==0) term2=log(kx) else term2=kx^ps[i2]
    }
  }
  # Fit the model, extract parameters
  estpar <- glm(ky ~ term1 + term2)$coefficients
  # plotting the fit
  x <- unique(kx)
  # x <- seq(1,(max(kx)+1), by = 1e-2)
  if (ps[i1] == 0) ya=log(x) else ya=x^ps[i1]
  if ((ps[i1] == 0) & (ps[i2] == 0)) yb=log(x)^2 else {
    if (i2 == i1) yb=x^ps[i1]*log(x) else{ 
      if (ps[i2]==0) yb=log(x) else yb=x^ps[i2]
    }
  }
  y <- estpar[1] + estpar[2]*ya + estpar[3]*yb
  return(y)
}

#' Find best fit fractional polynomial using GLM
#' 
#' @param x indepedent variable to find best fit fractional powers
#' @param y dependent variable, no transformation is done in the code
#' @param ... arguments to glm()
#' 
#' @export
fractional_poly <- function(x, y, verbose=FALSE,...) {
    ps=c(-2,-1,-0.5,0,0.5,1,2,3)
    for (i in 1:8) {
      for (j in i:8) {
        if (ps[i] == 0) term1=log(x) else term1=x^ps[i]
        if ((ps[i] == 0) & (ps[j] == 0)) term2=log(x)^2 
        else {
          if (j == i) term2=x^ps[i]*log(x) else {
            if (ps[j]==0)  term2=log(x) else term2=x^ps[j]
          }
        }
        aic = AIC(glm(y ~ term1 + term2, ...))
        if (verbose) cat(i,j,aic,"\n")
        if (i*j == 1) aicmin = c(i,j,aic) else {
          if (aic < aicmin[3]) aicmin = c(i,j,aic)
        }
      }
    }
    i1 = aicmin[1]
    i2 = aicmin[2]
    terms <- character(2)
    if (ps[i1] == 0) {
        term1=log(x); terms[1] <- paste('log(x)')
    } else {
        term1=x^ps[i1]; terms[1] <- paste('x^', ps[i1])
    }
    if ((ps[i1] == 0) & (ps[i2] == 0)) {
        term2=log(x)^2; terms[2] <- paste('log(x)^2')
    } else {
      if (i2 == i1) {
        term2=x^ps[i1]*log(x); terms[2] <- paste('x^', ps[i1], '* log(x)')
      } else { 
        if (ps[i2]==0) {
            term2=log(x); terms[2] <- paste('log(x)') 
        } else {
            term2=x^ps[i2]; terms[2] <- paste('x^', ps[i2])
        }
      }
    }
    fitx = glm(y~term1+term2, ...)
    message(terms)
    invisible(list(fit=fitx, terms=terms))
}