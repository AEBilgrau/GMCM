#include <RcppArmadillo.h>

using namespace Rcpp;
// [[Rcpp::depends(RcppArmadillo)]]

// Auxiliary functions as pow(x, n) is slow 

inline double square(double x) {
  return x*x;
}

inline double cube(double x) {
  return (x*x)*x;
}


// [[Rcpp::export]]
arma::mat dmvnormal(arma::mat& x, arma::rowvec mu, arma::mat sigma) {
  int m = x.n_cols;
  
  // sympd tells inv that sigma is symmetric and postive definte
  arma::mat invsig = arma::inv_sympd(sigma); 
  arma::mat y(x.n_rows, m, arma::fill::none);
  
  // Computing mahalanobis distance
  for (int j=0; j<m; j++) {
    y(arma::span::all, j) = x.col(j) - mu(j);
  }
  arma::vec dist = sum(((y * invsig) % y), 1);
  
  // Computing log of the determinant
  double sign = 1.0f;
  double logdet = 1.0f;
  log_det(logdet, sign, sigma);
  
  // Computing log normal density
  return exp(-(m*log(2.0f*arma::datum::pi) + logdet + dist)/2.0f);
}


// [[Rcpp::export]]
arma::colvec dgmm_loglik(Rcpp::List mus, 
                         Rcpp::List sigmas, 
                         Rcpp::NumericVector pie, 
                         arma::mat& z, 
                         bool marginal_loglik) {

  int d = mus.size();
  int n = z.n_rows;
  int m = z.n_cols;

  arma::colvec ans(n, arma::fill::zeros);
  arma::rowvec tmp_mu(m, arma::fill::none);
  arma::mat tmp_sigma(m, m, arma::fill::none);
  for (int k=0; k<d; k++) {
    tmp_mu = as<arma::rowvec>(Rcpp::wrap(mus[k]));
    tmp_sigma = as<arma::mat>(Rcpp::wrap(sigmas[k]));

    ans = ans + pie[k]*dmvnormal(z, tmp_mu, tmp_sigma);
  }
  ans = arma::log(ans);
  
  if (!marginal_loglik) {
    ans = arma::sum(ans);
  }
  return(ans);
}



// [[Rcpp::export]]
arma::mat dgmm_loglik_marginal(Rcpp::List mus, 
                               Rcpp::List sigmas, 
                               NumericVector pie, 
                               arma::mat& z, 
                               bool marginal_loglik) {
  int d = mus.size();
  int n = z.n_rows;
  int m = z.n_cols;

  arma::mat ans(n, m, arma::fill::none);
  arma::colvec marginal_eval(n, arma::fill::zeros);
  
  arma::mat z_tmp(n, 1, arma::fill::none);
  
  // Get k'th mu and variance for the j'th marginal
  arma::rowvec tmp_mu(1, arma::fill::none);
  arma::mat tmp_sigma(1, 1, arma::fill::none);
  
  for (int j=0; j<m; j++) {    
    marginal_eval(arma::span::all) = arma::zeros<arma::colvec>(n);
   
    // Get j'th column
    z_tmp = z(arma::span::all, j);
    
    for (int k=0; k<d; k++) {
 
      tmp_mu(0) = as<arma::rowvec>(Rcpp::wrap(mus[k]))(j);
      tmp_sigma(0,0) = as<arma::mat>(Rcpp::wrap(sigmas[k]))(j,j);

      // Evalutate the marginal k'th component for the j'th marginal
      marginal_eval += pie[k]*dmvnormal(z_tmp, tmp_mu, tmp_sigma);

    }
    ans(arma::span::all, j) = arma::log(marginal_eval);
  }

  if (!marginal_loglik) {
    ans = arma::sum(ans);
  }
  return ans;
}


// Abramowitz, Stegun p. 299 (7.1.25) (using error function) improved
// [[Rcpp::export]]
Rcpp::NumericVector approx_pnorm(Rcpp::NumericVector& z, 
                                 const double mu, 
                                 const double sd) {
  int n = z.size();
  const double a1 =  0.3480242;
  const double a2 = -0.0958798;
  const double a3 =  0.7478556;
  const double p  =  0.47047; 
  const double sqrt2 = 1.4142136;
  double t, zi, erfzi, sign;
  
  NumericVector ans = no_init(n);
  for (int i=0; i<n; i++) {
    zi = (z(i) - mu)/(sd*sqrt2);
    if (zi < 0.0) {
      zi = -1.0f*zi; 
      t = 1.0f/(1.0f + p*zi);
      ans(i) = 0.5f*(a1*t + a2*square(t) + a3*cube(t))*exp(-square(zi));
    } else {
      t = 1.0f/(1.0f + p*zi);
      ans(i) = 1.0f-0.5f*(a1*t + a2*square(t) + a3*cube(t))*exp(-square(zi));
    }
  }
  return ans;
}

// Approximate univariate Gaussian CDF
//// [[Rcpp::export]]
//Rcpp::NumericVector approx_pnorm2(Rcpp::NumericVector& z, 
//                                 double mu, 
//                                 double sd) {
//  int n = z.size();
//  double exp_y;
//  double a1 = 1.5976;
//  double a2 = 0.070565992;
//  double zi;
//  
//  NumericVector ans = no_init(n);
//  for (int i=0; i<n; i++) {
//    zi = (z(i) - mu)/sd;
//    exp_y = exp(a1*zi + a2*pow(zi, 3));
//    ans(i) = 1 - pow(1 + exp_y, -1);
//  }
//  return ans;
//}

//Rcpp::NumericVector approx_pnorm(Rcpp::NumericVector& z, 
//                                 double mu, 
//                                 double sd) {
//  int n = z.size();
//  double exp_minusy;
//  double fac = 1.5976;
//  double zi;
//  
//  NumericVector ans = no_init(n);
//  for (int i=0; i<n; i++) {
//    zi = (z(i) - mu)/sd;
//    exp_minusy = exp(-fac*zi*(1 + 0.04417*pow(zi,2)));
//    ans(i) = 1/(exp_minusy + 1);
//  }
//  return ans;
//}

// Approximate marginals of Gaussian mixture model CDF
// [[Rcpp::export]]
arma::mat pgmm_marginal(arma::mat& z,
                        Rcpp::List mus, 
                        Rcpp::List sigmas, 
                        NumericVector pie) {
  int d = mus.size();
  int n = z.n_rows;
  int m = z.n_cols;
  
  NumericMatrix x = Rcpp::as<Rcpp::NumericMatrix>(wrap(z));
  NumericMatrix tmp(n, m); // Matrix of n rows and m columns (filled with 0)

  // Holders for the k'th mu and variance for the j'th marginal
  NumericVector tmp_mus(m);
  NumericMatrix tmp_sigmas(m, m);
    
  double mu, sd;
  NumericVector xx = no_init(n);
  
  for (int k=0; k<d; k++) {
    tmp_mus = as<NumericVector>(wrap(mus[k]));
    tmp_sigmas = as<NumericMatrix>(wrap(sigmas[k]));
    
    for (int j=0; j<m; j++) { 
      xx = x(_, j);
      mu = tmp_mus(j);
      sd = sqrt(tmp_sigmas(j,j));
      tmp(_, j) = tmp(_, j) + pie[k]*approx_pnorm(xx, mu, sd);
    }
  }
  arma::mat ans(tmp.begin(), n, m, false); 

  return ans;
}


