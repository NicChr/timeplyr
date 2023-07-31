#include <Rcpp.h>
using namespace Rcpp;

// Internal function
// x must be a numeric or integer
// threshold must be a numeric or integer of length 1
// [[Rcpp::export]]
IntegerVector roll_time_threshold(NumericVector x, double threshold = 1,
                                  bool switch_on_boundary = true) {
  int n = x.length();
  if (n == 0) {
    return IntegerVector(0);
  }
  double tol = sqrt(std::numeric_limits<double>::epsilon());
  IntegerVector out(n);
  double init_threshold = threshold;
  if (switch_on_boundary == true){
    tol = -tol;
  }
  for (int i = 0; i < n; ++i) {
    if (NumericVector::is_na(x[i])){
      out[i] = NA_INTEGER;
    } else if ((x[i] - threshold) > tol) {
      out[i] = 1;
      threshold = init_threshold + x[i];
    }
  }
  return out;
}
