#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector roll_int_threshold(IntegerVector x, int threshold = 1L,
                                  bool switch_on_boundary = true) {
  if (x.length() == 0) {
    return IntegerVector(0);
  }
  IntegerVector out(x.length());
  int init_threshold = threshold;
  LogicalVector x_na = is_na(x);
  if (switch_on_boundary == true){
    for (int i = 0; i < x.length(); ++i) {
      if (!x_na[i] && x[i] >= threshold) {
        out[i] = 1L;
        threshold = init_threshold + x[i];
      }
    }
  } else {
    for (int i = 0; i < x.length(); ++i) {
      if (!x_na[i] && x[i] > threshold) {
        out[i] = 1L;
        threshold = init_threshold + x[i];
      }
    }
  }

  out[x_na] = NA_INTEGER;
  return out;
}
