#include <Rcpp.h>
using namespace Rcpp;

// Internal function
// x must be a numeric or integer
// threshold must be a numeric or integer of length 1
// This basically cumulatively sums x and once a threshold is reached
// This is flagged a 1, the counting resets and another cumulative sum occurs
// and so on and so on
// [[Rcpp::export(rng = false)]]
IntegerVector roll_time_threshold(SEXP x, double threshold = 1,
                                  bool switch_on_boundary = true) {
  int n = Rf_length(x);
  IntegerVector out(n);
  double init_threshold = 0;
  init_threshold = init_threshold + threshold;
  switch( TYPEOF(x) ) {
  case REALSXP: {
    NumericVector xv = Rcpp::as<Rcpp::NumericVector>(x);
    double tol = sqrt(std::numeric_limits<double>::epsilon());
    if (switch_on_boundary == true){
      tol = -tol;
    }
    for (int i = 0; i < n; ++i) {
      if (NumericVector::is_na(xv[i])){
        out[i] = NA_INTEGER;
      } else if ((xv[i] - threshold) > tol) {
        out[i] = 1;
        threshold = init_threshold + xv[i];
      }
    }
    break;
  }
  case INTSXP: {
    IntegerVector xv = Rcpp::as<Rcpp::IntegerVector>(x);
    if (switch_on_boundary == true){
      for (int i = 0; i < n; ++i) {
        if (IntegerVector::is_na(xv[i])){
          out[i] = NA_INTEGER;
        } else if (xv[i] >= threshold) {
          out[i] = 1;
          threshold = init_threshold + xv[i];
        }
      }
    } else {
      for (int i = 0; i < n; ++i) {
        if (IntegerVector::is_na(xv[i])){
          out[i] = NA_INTEGER;
        } else if (xv[i] > threshold) {
          out[i] = 1;
          threshold = init_threshold + xv[i];
        }
      }
    }
    break;
  }
  default: {
    Rcpp::stop("roll_time_threshold only supports integer and numeric vectors");
  }
  }
  return out;
}
