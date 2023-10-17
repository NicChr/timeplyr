#include <Rcpp.h>
using namespace Rcpp;

// bool cpp_double_equal_strict(double x, double y, double tolerance){
//   double ax = std::fabs(x);
//   double ay = std::fabs(y);
//   bool both_zero = ( ax < tolerance ) && ( ay < tolerance );
//   double adiff = std::fabs(x - y);
//   double rdiff = adiff / std::max(ax, ay);
//   bool out = both_zero || (
//     (adiff < tolerance) && (rdiff < tolerance)
//   );
//   return out;
// }
// Whole number check
// NA values are ignored
// bool is_whole_num(SEXP x, Nullable<NumericVector> tol) {
//   double tolerance = std::sqrt(std::numeric_limits<double>::epsilon());
//   if (tol.isNotNull()){
//     tolerance = Rcpp::as<double>(tol);
//   }
//   bool is_whole = true;
//   double rdiff;
//   double adiff;
//   double rounded;
//   bool out = false;
//   double diff_zero;
//   bool is_zero;
//   switch ( TYPEOF(x) ){
//   case LGLSXP:
//   case INTSXP: {
//     out = true;
//     break;
//   }
//   case REALSXP: {
//     // Re-initialise so that we can break when we find non-whole num
//     out = true;
//     int n = Rf_length(x);
//     double *p_x = REAL(x);
//     for (int i = 0; i < n; ++i) {
//       is_whole = cpp_double_equal_strict(std::round(p_x[i]), p_x[i]);
//       if (!is_whole && !(p_x[i] != p_x[i])){
//         out = false;
//         break;
//       }
//     }
//     break;
//   }
//   }
//   return out;
// }
// [[Rcpp::export(rng = false)]]
bool is_whole_num(SEXP x, Nullable<NumericVector> tol) {
  double tolerance = std::sqrt(std::numeric_limits<double>::epsilon());
  if (tol.isNotNull()){
    tolerance = Rcpp::as<double>(tol);
  }
  bool is_whole = true;
  double rdiff;
  double adiff;
  double rounded;
  bool out = false;
  double diff_zero;
  bool is_zero;
  switch ( TYPEOF(x) ){
  case LGLSXP:
  case INTSXP: {
    out = true;
    break;
  }
  case REALSXP: {
    // Re-initialise so that we can break when we find non-whole num
    out = true;
    int n = Rf_length(x);
    double *p_x = REAL(x);
    for (int i = 0; i < n; ++i) {
      diff_zero = std::fabs(p_x[i]);
      is_zero = diff_zero < tolerance;
      rounded = std::round(p_x[i]);
      adiff = std::fabs(rounded - p_x[i]);
      rdiff = adiff / std::max(std::fabs(p_x[i]), std::fabs(rounded));
      is_whole = is_zero || ( (adiff < tolerance) && (rdiff < tolerance) );
      if (!is_whole && !(p_x[i] != p_x[i])){
        out = false;
        break;
      }
    }
    break;
  }
  }
  return out;
}
// bool is_whole_num(SEXP x, Nullable<NumericVector> tol) {
//   double tolerance = std::sqrt(std::numeric_limits<double>::epsilon());
//   if (tol.isNotNull()){
//     tolerance = Rcpp::as<double>(tol);
//   }
//   bool has_decimal = false;
//   double rdiff;
//   double adiff;
//   double rounded;
//   bool out = false;
//   double diff_zero;
//   bool is_zero;
//   switch ( TYPEOF(x) ){
//   case LGLSXP:
//   case INTSXP: {
//     out = true;
//     break;
//   }
//   case REALSXP: {
//     // Re-initialise so that we can break when we find non-whole num
//     out = true;
//     int n = Rf_length(x);
//     double *p_x = REAL(x);
//     for (int i = 0; i < n; ++i) {
//       diff_zero = std::fabs(p_x[i] - 0);
//       is_zero = diff_zero < tolerance;
//       rounded = std::round(p_x[i]);
//       adiff = std::fabs(rounded - p_x[i]);
//       rdiff = adiff / std::max(std::abs(p_x[i]), std::abs(rounded));
//       has_decimal = !is_zero && (!(rdiff < tolerance));
//       if (has_decimal && !(p_x[i] != p_x[i])){
//         out = false;
//         break;
//       }
//     }
//     break;
//   }
//   }
//   return out;
// }
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
