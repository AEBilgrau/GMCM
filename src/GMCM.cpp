// We only include RcppArmadillo.h which pulls Rcpp.h in for us
#include <RcppArmadillo.h>

//using namespace Rcpp;
//using namespace RcppArmadillo;
//// [[Rcpp::depends(RcppArmadillo)]]

// Auxiliary functions as pow(x, n) is somewhat slow 
// We make the radical assumption that a*b equals b*a, which does 
// not hold in general for floating point arithmetic.

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
  for (int j=0; j<m; ++j) {
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
arma::mat rmvnormal(const int n, arma::rowvec mu, arma::mat sigma) {
  Rcpp::RNGScope();
  const int d = mu.size();
  
  // Create matrix of standard normal random values
  arma::mat ans(n, d, arma::fill::none);
  for (int j = 0; j < d; ++j) {  // Fill ans with random values
    ans.col(j) = Rcpp::as<arma::colvec>(Rcpp::rnorm(n));
  }

  // Do the Cholesky decomposition
  const arma::mat csigma = arma::chol(sigma);
  
  // Do the transformation
  ans = ans * csigma;
  ans.each_row() += mu; // Add mu to each row in transformed ans

  return ans;  
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
  for (int k=0; k<d; ++k) {
    tmp_mu = Rcpp::as<arma::rowvec>(Rcpp::wrap(mus[k]));
    tmp_sigma = Rcpp::as<arma::mat>(Rcpp::wrap(sigmas[k]));

    ans = ans + pie[k]*dmvnormal(z, tmp_mu, tmp_sigma);
  }
  ans = arma::log(ans);
  
  if (!marginal_loglik) {
    ans = arma::sum(ans);
  }
  return ans;
}


// [[Rcpp::export]]
arma::mat dgmm_loglik_marginal(Rcpp::List mus, 
                               Rcpp::List sigmas, 
                               Rcpp::NumericVector pie, 
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
  
  for (int j=0; j<m; ++j) {    
    marginal_eval(arma::span::all) = arma::zeros<arma::colvec>(n);
   
    // Get j'th column
    z_tmp = z(arma::span::all, j);
    
    for (int k=0; k<d; ++k) {
 
      tmp_mu(0) = Rcpp::as<arma::rowvec>(Rcpp::wrap(mus[k]))(j);
      tmp_sigma(0,0) = Rcpp::as<arma::mat>(Rcpp::wrap(sigmas[k]))(j,j);

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


// Approximate univariate Gaussian CDF, applid marginally 
// Abramowitz, Stegun p. 299 (7.1.25) (using error function) improved.
// [[Rcpp::export]]
arma::colvec approx_pnorm(arma::colvec& z, const double mu, const double sd) {
  const int n = z.size();
  const double a1 =  0.3480242;
  const double a2 = -0.0958798;
  const double a3 =  0.7478556;
  const double p  =  0.47047; 
  const double sqrt2 = 1.4142136;
  
  arma::colvec ans(n, arma::fill::none);

  for (int i = 0; i < n; ++i) {
    double zi = (z(i) - mu)/(sd*sqrt2);
    if (zi < 0.0) {
      zi = -1.0*zi; 
      const double t = 1.0/(1.0 + p*zi);
      ans(i) = 0.5*(a1*t + a2*square(t) + a3*cube(t))*exp(-square(zi));
    } else {
      const double t = 1.0/(1.0 + p*zi);
      ans(i) = 1.0-0.5*(a1*t + a2*square(t) + a3*cube(t))*exp(-square(zi));
    }
  }

  return ans;
}

// Approximate univariate Gaussian CDF, applied marginally 
// // [[Rcpp::export]]
// Rcpp::NumericVector approx_pnorm2(Rcpp::NumericVector& z, 
//                                  const double mu, 
//                                  const double sd) {
//   const int n = z.size();
//   const double a1 =  0.3480242;
//   const double a2 = -0.0958798;
//   const double a3 =  0.7478556;
//   const double p  =  0.47047; 
//   const double sqrt2 = 1.4142136;
  
//   Rcpp::NumericVector ans = Rcpp::no_init(n);
//   for (int i = 0; i < n; ++i) {
//     double zi = (z(i) - mu)/(sd*sqrt2);
//     if (zi < 0.0) {
//       zi = -1.0*zi; 
//       const double t = 1.0/(1.0 + p*zi);
//       ans(i) = 0.5*(a1*t + a2*square(t) + a3*cube(t))*exp(-square(zi));
//     } else {
//       const double t = 1.0/(1.0 + p*zi);
//       ans(i) = 1.0-0.5*(a1*t + a2*square(t) + a3*cube(t))*exp(-square(zi));
//     }
//   }
//   return ans;
// }

// Approximate univariate Gaussian CDF, applied marginally 
//// [[Rcpp::export]]
//Rcpp::NumericVector approx_pnorm3(Rcpp::NumericVector& z, 
//                                 double mu, 
//                                 double sd) {
//  int n = z.size();
//  double exp_y;
//  double a1 = 1.5976;
//  double a2 = 0.070565992;
//  double zi;
//  
//  NumericVector ans = no_init(n);
//  for (int i=0; i<n; ++i) {
//    zi = (z(i) - mu)/sd;
//    exp_y = exp(a1*zi + a2*pow(zi, 3));
//    ans(i) = 1 - pow(1 + exp_y, -1);
//  }
//  return ans;
//}

// Approximate univariate Gaussian CDF, applied marginally 
//Rcpp::NumericVector approx_pnorm4(Rcpp::NumericVector& z, 
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
                        Rcpp::NumericVector pie) {

  const int d = mus.size(); // Nbr of components in mixture (not dimension!)
  const arma::uword n = z.n_rows;   // Nbr of observations
  const arma::uword m = z.n_cols;   // Dimension (!)
  
  arma::mat ans(n, m, arma::fill::zeros); // Matrix of n rows and m columns (filled with 0)

  for (int k=0; k<d; ++k) {
    // Holders for the k'th mu and variance for the j'th marginal
    arma::colvec tmp_mus = Rcpp::as<arma::colvec>(Rcpp::wrap(mus[k]));
    arma::mat tmp_sigmas = Rcpp::as<arma::mat>(Rcpp::wrap(sigmas[k]));
    
    for (arma::uword j=0; j<m; ++j) { 

      const double mu = tmp_mus(j);
      const double sd = sqrt(tmp_sigmas(j,j));
      arma::colvec z_col_j = z(arma::span::all, j);
      arma::colvec approx_pn = approx_pnorm(z_col_j, mu, sd);
      ans.col(j) = ans.col(j) + pie[k] * approx_pn;

    }
  }

  return ans;
}

// Alternative (original) implementation
// // [[Rcpp::export]]
// arma::mat pgmm_marginal2(arma::mat& z,
//                          Rcpp::List mus, 
//                          Rcpp::List sigmas, 
//                          Rcpp::NumericVector pie) {

//   const int d = mus.size(); // Nbr of components in mixture (not dimension!)
//   const arma::uword n = z.n_rows;   // Nbr of observations
//   const arma::uword m = z.n_cols;   // Dimension (!)
  
//   Rcpp::NumericMatrix x = Rcpp::as<Rcpp::NumericMatrix>(Rcpp::wrap(z));
//   Rcpp::NumericMatrix tmp_ans(n, m); // Matrix of n rows and m columns (filled with 0)

//   for (int k=0; k<d; ++k) {
//     // Holders for the k'th mu and variance for the j'th marginal
//     Rcpp::NumericVector tmp_mus = Rcpp::as<Rcpp::NumericVector>(Rcpp::wrap(mus[k]));
//     Rcpp::NumericMatrix tmp_sigmas = Rcpp::as<Rcpp::NumericMatrix>(Rcpp::wrap(sigmas[k]));
    
//     for (arma::uword j=0; j<m; ++j) { 
//       Rcpp::NumericVector xx = Rcpp::no_init(n);
//       xx = x(Rcpp::_, j);
//       const double mu = tmp_mus(j);
//       const double sd = sqrt(tmp_sigmas(j,j));
//       tmp_ans(Rcpp::_, j) = tmp_ans(Rcpp::_, j) + pie[k] * approx_pnorm2(xx, mu, sd);
//     }
//   }
//   arma::mat ans(tmp_ans.begin(), n, m, false); 

//   return ans;
// }

// arma::mat EStepRcpp (arma::mat& z
//                      Rcpp::List mus, 
//                      Rcpp::List sigmas, 
//                      Rcpp::NumericVector pie) {
//   WRITE EStep in Rcpp
// }

// Rcpp::List MStepRcpp (arma::mat& z
//                      arma::mat& kappa, 
//                      bool meta_special_case) {
//   WRITE MStep in Rcpp
// }



/*** R
# For debugging
library(GMCM)
data <- matrix(runif(200), 100, 2)
theta <- rtheta(d = 2, m = 2)
aa <- pgmm_marginal(data, theta$mu, theta$sigma, theta$pie)
#bb <- pgmm_marginal2(data, theta$mu, theta$sigma, theta$pie)
#identical(aa, bb)
#max(aa - bb)

for (i in 1:50000)
  aa <- pgmm_marginal(data, theta$mu, theta$sigma, theta$pie)


*/

