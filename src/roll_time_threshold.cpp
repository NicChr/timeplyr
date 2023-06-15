#include <Rcpp.h>
using namespace Rcpp;

// Internal function
// x must be a numeric or integer
// threshold must be a numeric or integer of length 1
// [[Rcpp::export]]
IntegerVector roll_time_threshold(NumericVector x, double threshold = 1) {
  if (x.length() == 0) {
    return IntegerVector(0);
  }
  IntegerVector out(x.length());
  double init_threshold = threshold;

  for (int i = 0; i < x.length(); ++i) {
    if (!NumericVector::is_na(x[i]) && x[i] >= threshold) {
      out[i] = 1;
      threshold = init_threshold + x[i];
    }
  }
  return out;
}
