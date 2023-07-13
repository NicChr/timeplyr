#include <Rcpp.h>
using namespace Rcpp;

// Internal function
// x must be a numeric or integer
// threshold must be a numeric or integer of length 1
// [[Rcpp::export]]
IntegerVector roll_time_threshold(NumericVector x, double threshold = 1,
                                  bool switch_on_boundary = true) {
  if (x.length() == 0) {
    return IntegerVector(0);
  }
  double tol = sqrt(std::numeric_limits<double>::epsilon());
  IntegerVector out(x.length());
  double init_threshold = threshold;
  LogicalVector x_na = is_na(x);
  if (switch_on_boundary == true){
    tol = -tol;
  }
  for (int i = 0; i < x.length(); ++i) {
    if (!x_na[i] && (x[i] - threshold > tol)) {
      out[i] = 1L;
      threshold = init_threshold + x[i];
    }
  }
  out[x_na] = NA_INTEGER;
  return out;
}
