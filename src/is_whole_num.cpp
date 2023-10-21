#include <Rcpp.h>
using namespace Rcpp;

// Whole number check
// NA values are ignored

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
      rdiff = adiff / std::fmax(std::fabs(p_x[i]), std::fabs(rounded));
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
// Vectorised version (unnecessary)
// bool is_whole_num2(SEXP x, SEXP tol) {
//   int x_len = Rf_length(x);
//   int tol_len = Rf_length(tol);
//   int n = std::max(x_len, tol_len);
//   if (x_len <= 0 || tol_len <= 0){
//     n = 0;
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
//     double *p_x = REAL(x);
//     double *p_t = REAL(tol);
//     int xi;
//     int ti;
//     for (int i = 0; i < n; ++i) {
//       xi = (i % x_len);
//       ti = (i % tol_len);
//       diff_zero = std::fabs(p_x[xi]);
//       is_zero = diff_zero < p_t[ti];
//       rounded = std::round(p_x[xi]);
//       adiff = std::fabs(rounded - p_x[xi]);
//       rdiff = adiff / std::fmax(std::fabs(p_x[xi]), std::fabs(rounded));
//       is_whole = is_zero || ( (adiff < p_t[ti]) && (rdiff < p_t[ti]) );
//       if (!is_whole && !(p_x[xi] != p_x[xi])){
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
