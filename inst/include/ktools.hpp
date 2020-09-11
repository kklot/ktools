#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>

#pragma once

namespace ktools {
  
#define _eps 1e-12 // An alternative limit argument for the first-order IGRMF

// lag difference
template <class Type>
vector<Type> diff(vector<Type> x, Type order=1) {
  int n = CppAD::Integer(x.size() - order);
  vector<Type> y(n);
  if (order==1)
    for (int i = 0; i < y.size(); ++i)
      y(i) = x(i+1) - x(i);
  if (order==2)
    for (int i = 0; i < y.size(); ++i)
      y(i) = x(i) - 2*x(i+1) + x(i+2);
  return(y);
}

// logit
template <class Type>
Type logit(Type p) {
  if ((p < 0) | (p > 1)) {
    Rcout << "p must be in [0, 1]";
    return 0;
  }
  return log(p/(1-p));
}

// inverse logit
template <class Type>
Type logit_inv(Type x) {
  return 1/(1+exp(-x));
}

// Gumbel density
template <class Type>
Type dgumbel(Type x, Type location=0, Type scale=1, bool give_log=true) {
  if (scale<=0) Rf_error("Invalid scale");
  Type o = exp((location-x)/scale) * exp(-exp((location-x)/scale))/scale;
  return (give_log) ? log(o) : o;
}

// uniform density
template <class Type>
Type dunif(Type x, Type min=0.0, Type max=1.0, bool give_log=true) {
  bool inside = (x >= min & x <= max) ? 1 : 0;
  Type o = (inside) ? 1/(max-min) : 0.0;
  return (give_log) ? log(o) : o;
}

// soft constraint to zero
template <class Type>
Type soft_zero_sum(vector<Type> x) {
    return dnorm(sum(x), Type(0.0), Type(0.001) * x.size(), true);
}

// Log-logistic distribution
// 
// survival function
template <class Type>
Type St_llogis(Type t, Type alpha, Type lambda) {
  return Type(1.0) / (Type(1.0) + pow(lambda * t, alpha));
}

// density function
template <class Type>
Type ft_llogis(Type t, Type alpha, Type lambda) {
  Type num = lambda * alpha * pow(lambda*t, alpha-Type(1.0));
  Type St  = St_llogis(t, alpha, lambda);
  return num * pow(St, 2);
}

// Log-logistic distribution type I
// 
// survival function
template <class Type>
Type St_llogisI(Type t, Type alpha, Type lambda, Type a) {
  Type term = pow(t * lambda, -alpha);
  return 1 - 1 / pow(1 + term, a);
}

// density function
template <class Type>
Type ft_llogisI(Type t, Type alpha, Type lambda, Type a) {
  Type term = pow(t * lambda, -alpha);
  return (1/t) * (a * alpha * term) / pow(1 + term, a + 1); 
}

// Mean function
template <class Type>
Type mu_llogis(Type alpha, Type lambda) {
  Type t1 = Type(1.0)/lambda;
  Type t2 = (M_PI/alpha)/sin(M_PI/alpha);
  return t1*t2;
}

// penalized-log precision density
template <class Type>
Type pc_prec(Type x, Type u = 1.0, Type alpha = 0.01, bool give_log=true) {
  Type lambda = -log(alpha) / u, 
       s      = 1/sqrt(x);
  Type d      = dexp(s, lambda, 1), 
       logj   = -log(2) - 1.5 * log(x);
  if (give_log)
    return d + logj;
  else
    return d * exp(logj);
}

// IID
template <class Type>
Type iid(vector<Type> x, Type tau, bool sum_c=true) {
  Type o = Type(0.0);
  o -= pc_prec(tau, Type(1));
  if (sum_c) 
    o -= soft_zero_sum(x);
  o -= dnorm(x, Type(0.0), pow(tau, -0.5), true).sum();
  return o;
}

// prepare Q
template <class Type>
Eigen::SparseMatrix<Type> prepare_Q(matrix<Type> R, Type tau) {
  R = tau * R.array();
  R.diagonal().array() += _eps;
  return tmbutils::asSparseMatrix(R);
}

// RW
template <class Type>
Type rw(vector<Type> x, matrix<Type> R, Type tau, Type order=2,
        bool sum_c=true, bool slope_c=false) {
  Type rwll = Type(0.0);
  if (sum_c)
    rwll -= soft_zero_sum(x);
  if (slope_c) {
    vector<Type> xid(x.size());
    for (int i = 0; i < xid.size(); ++i) xid(i) = i+1;
    xid = x * xid;
    rwll -= soft_zero_sum(xid);
  }
  rwll += density::GMRF(prepare_Q(R, tau))(x);
  rwll -= 0.5 * order * log(2*M_PI); // corrected for rank deficiency
  return rwll;
}

// BESAG
template <class Type>
Type besag(matrix<Type> R, Type tau, vector<Type> u_vec) {
  Type dll = 0.0;
  dll -= pc_prec(tau); // TODO: allow to specify this
  dll += density::GMRF(prepare_Q(R, tau))(u_vec);
  // find singletons
  // R.rowwise(). 
  dll -= soft_zero_sum(u_vec);
  return dll;
}

// BYM Model
template <class Type>
Type bym(matrix<Type> R, Type tau_u, Type tau_v, 
  vector<Type> u_vec, vector<Type> v_vec) {
  Type dll = 0.0;
  dll += iid(v_vec, tau_v);
  dll += besag(R, tau_u, u_vec);
  return dll;
}

// DEAN Model: TODO find an example and compare
template <class Type>
Type dean(matrix<Type> R, Type tau, Type phi, 
  vector<Type> u_vec, vector<Type> v_vec) {
  Type dll = 0.0;
  dll += iid(v_vec, tau/(1-phi));
  dll += besag(R, tau/phi, u_vec);
  return dll;
}

// Leroux model: TODO find an example and compare
template <class Type>
Type leroux(matrix<Type> R, Type tau, Type phi, 
  vector<Type> u_vec, vector<Type> v_vec) {
  Type dll = 0.0;
  dll += iid(v_vec, tau*(1-phi));
  dll += besag(R, tau*phi);
  return dll;
}

// BYM2

} // End namespace