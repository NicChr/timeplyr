#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector roll_int_threshold(IntegerVector x, int threshold = 1,
                                  bool switch_on_boundary = true) {
  int n = x.length();
  if (n == 0) {
    return IntegerVector(0);
  }
  IntegerVector out(n);
  int init_threshold = threshold;
  if (switch_on_boundary == true){
    for (int i = 0; i < n; ++i) {
      if (IntegerVector::is_na(x[i])){
        out[i] = NA_INTEGER;
      } else if (x[i] >= threshold) {
        out[i] = 1;
        threshold = init_threshold + x[i];
      }
    }
  } else {
    for (int i = 0; i < n; ++i) {
      if (IntegerVector::is_na(x[i])){
        out[i] = NA_INTEGER;
      } else if (x[i] > threshold) {
        out[i] = 1;
        threshold = init_threshold + x[i];
      }
    }
  }
  return out;
}
