// We only include RcppArmadillo.h which pulls Rcpp.h in for us
#include <RcppArmadillo.h>


// Auxiliary functions as pow(x, n) is somewhat slow. We make the radical
// assumption that a*b equals b*a, which does not hold in general for
// floating point arithmetic.

inline double square(double x) {
  return x*x;
}

inline double cube(double x) {
  return (x*x)*x;
}

// [[Rcpp::export]]
arma::rowvec colSdsArma(const arma::mat & X, const int norm_type = 0) {
  return stddev(X, norm_type, 0);
}

// [[Rcpp::export]]
arma::colvec rowSdsArma(const arma::mat & X, const int norm_type = 0) {
  return stddev(X, norm_type, 1);
}

//' Multivariate Gaussian density and simulation
//'
//' Fast simulation from and evaluation of multivariate Gaussian probability
//' densities.
//'
//' \code{dmvnormal} functions similarly to \code{dmvnorm} from the
//' \code{mvtnorm}-package and likewise for \code{rmvnormal} and
//' \code{rmvnorm}.
//'
//' @aliases dmvnormal rmvnormal
//' @param x A \code{p} times \code{k} matrix of quantiles. Each rows
//'   correspond to a realization from the density and each column corresponds
//'   to a dimension.
//' @param mu The mean vector of dimension \code{k}.
//' @param sigma The variance-covariance matrix of dimension \code{k} times
//'   \code{k}.
//' @return \code{dmvnormal} returns a \eqn{1} by \eqn{p} matrix of the
//'   probability densities corresponding to each row of \code{x}.
//' \code{sigma}. Each row corresponds to an observation.
//' @author Anders Ellern Bilgrau
//' @seealso \code{dmvnorm} and \code{rmvnorm} in the \code{mvtnorm}-package.
//' @examples
//' dmvnormal(x = matrix(rnorm(300), 100, 3),
//'           mu = 1:3,
//'           sigma = diag(3))
//' @export
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

//' @rdname dmvnormal
//' @param n The number of observations to be simulated.
//' @return \code{rmvnormal} returns a \code{p} by \code{k} matrix of
//'   observations from a multivariate normal distribution with the given mean
//'   \code{mu} and covariance
//' @examples
//' rmvnormal(n = 10, mu = 1:4, sigma = diag(4))
//' @export
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

  // Matrix of n rows and m columns (filled with 0)
  arma::mat ans(n, m, arma::fill::zeros);

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

// [[Rcpp::export]]
arma::mat EStepRcpp(arma::mat& z,
                    Rcpp::List mus,
                    Rcpp::List sigmas,
                    Rcpp::NumericVector pie) {

  const int d = mus.size(); // Nbr of components in mixture (not dimension!)
  const int n = z.n_rows;   // Nbr of observations
  //const arma::uword m = z.n_cols;   // Dimension of obs. (nbr of studies)

  arma::mat kappa = arma::mat(n, d, arma::fill::none);

  for (int k=0; k<d; ++k) {
    // Holders for the mixture prop., mean, and covariance of component k
    arma::rowvec tmp_mu = Rcpp::as<arma::rowvec>(Rcpp::wrap(mus[k]));
    arma::mat tmp_sigma = Rcpp::as<arma::mat>(Rcpp::wrap(sigmas[k]));
    double tmp_pie = pie[k];

    kappa(arma::span::all, k) = tmp_pie * dmvnormal(z, tmp_mu, tmp_sigma);
  }

  const arma::colvec rowsums = arma::sum(kappa, 1);
  kappa.each_col() /= rowsums;

  // Handle NaN/Infs
  // Change non-finite elements to zero
  kappa.elem(find_nonfinite(kappa)).zeros();

  return kappa;

}
//
//Rcpp::List MStepRcppFull(arma::mat& z
//                         arma::mat& kappa,
//                         bool meta_special_case) {
//  int m = kappa.n_cols;
//  int d = z.n_cols;
//
//  arma::colvec pie = sum(kappa, 0); // column means
//  pie = pie/sum(pie);
//
//  Rcpp::List mu(m);
//  Rcpp::List sigma(m);
//  for (int j=0; j<m; ++j) {
//    arma::mat tmp_z = z.each_col() %= kappa.col(j);  // Element-wise product
//    mu[j] = sum(tmp_z, 0)/sum(kappa.col(j));
//
//    arma::colvec wt = kappa.col(j)/sum(kappa.col(j));
//
//    sigma[j] = ; //??
//  }
//
//  return  Rcpp::List::create(Rcpp::Named("m")     = m,
//                             Rcpp::Named("d")     = d,
//                             Rcpp::Named("pie")   = pie,
//                             Rcpp::Named("mu")    = mu,
//                             Rcpp::Named("sigma") = sigma);
//}

//MStep <- function (x, kappa, meta.special.case = FALSE) {
//  mus    <- lapply(1:ncol(kappa),
//                   function(j) {colSums(x*kappa[,j])/sum(kappa[,j])})
//  sigmas <- lapply(1:ncol(kappa),
//                   function(j) {cov.wt(x, kappa[,j], method = "ML")$cov})
//  names(mus)  <- names(sigmas) <- paste("comp", 1:ncol(kappa), sep = "")
//  pie <- colMeans(kappa)
//  pie <- pie/sum(pie)
//
//  if (meta.special.case) {
//    m      <- ncol(kappa)
//    d      <- ncol(x)
//    pie1    <- colMeans(kappa)[1]; names(pie1) <- NULL
//    mu2    <- mean(mus[[2]])
//    sigma2 <- sqrt(mean(diag(sigmas[[2]])))
//    rho2   <- (sum(sigmas[[2]])-d*sigma2^2)/(d*(d-1)*sigma2^2)
//    par    <- c(pie1 = pie1, mu = mu2, sigma = sigma2, rho = rho2)
//    return(meta2full(par, d = d))
//  }
//  return(list(m = ncol(kappa), d = ncol(x), pie = pie,
//              mu = mus, sigma = sigmas))
//}



/*** R
# For debugging
library(GMCM)
data <- matrix(runif(200), 100, 2)
theta <- rtheta(d = 2, m = 3)
*/

