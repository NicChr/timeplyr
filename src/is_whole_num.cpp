#include <Rcpp.h>
using namespace Rcpp;

// Whole number check
// NA values are ignored
// [[Rcpp::export]]
bool is_whole_num(NumericVector x, Nullable<NumericVector> tol) {
  double tolerance = sqrt(std::numeric_limits<double>::epsilon());
  if (tol.isNotNull()){
    tolerance = as<double>(tol);
  }
  bool has_decimal;
  double diff;
  bool out = true;
  for (int i = 0; i < x.length(); ++i) {
    diff = abs(round(x[i]) - x[i]);
    has_decimal = !(diff < tolerance);
    if (has_decimal && !NumericVector::is_na(x[i])){
      out = false;
      break;
    }
  }
  return out;
}
// Keeping this as a pedagogical example of how to use R functions in cpp
// bool is_whole_num(NumericVector x) {
//   Function get_tol("sqrt_double_eps");
//   NumericVector tol_val = get_tol();
//   double tol = Rcpp::as<double>(tol_val);
//   bool has_decimal;
//   double diff;
//   bool out = true;
//   for (int i = 0; i < x.length(); ++i) {
//     diff = abs(round(x[i]) - x[i]);
//     has_decimal = !(diff < tol);
//     if (has_decimal && !NumericVector::is_na(x[i])){
//       out = false;
//       break;
//     }
//   }
//   return out;
// }
